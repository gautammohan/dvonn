-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
------------------------------------------------------------------------------

module Game (Game, evalGame, dvonn, phase1, phase2, startState) where

import Control.Monad.State (StateT, get, gets, lift, modify, runStateT)
import Control.Monad.Except
  ( ExceptT
  , catchError
  , lift
  , liftEither
  , runExceptT
  , throwError
  )
import Control.Monad

import Data.List

import Board (calcWinner, apply)
import Defs
import Move
import IOInterface

-- | Lift IO actions into the Game monad
liftGame :: IO a -> Game a
liftGame = Control.Monad.State.lift . Control.Monad.Except.lift

-- | A Game represents computations on the gamestate, with possible errors, as
-- well as IO computations to receive moves from players and print updates on
-- the board state
type Game = StateT GameState (ExceptT GameError IO)

-- | Given a move, execute it if it is valid. Otherwise, ask for another move to
-- be inputted.
executeMove :: Move -> Game ()
executeMove m = do
  gs@(GameState b t _) <- get
  if validMove b m
    then do
      let newB = apply m b
          newT = getNextTurn newB (player m)
      modify (\gs -> gs {turn = newT, board = newB})
    else do
    printErr InvalidMove
    m <- getTurnInput
    executeMove m

-- | The initial state of a dvonn game
startState :: GameState
startState = GameState emptyDvonn Start Phase1

-- | Loop that rejects improperly parsed moves from stdin, returning a
-- well-formed move when a player enters it.
getTurnInput :: Game Move
getTurnInput = do
  b <- gets board
  t <- gets turn
  liftGame $ printBoard b
  liftGame $ putStr $ prompt t ++ "> "
  s <- liftGame getLine
  case liftEither $ parseMove s t of
    Left e -> printErr e >> getTurnInput
    Right m -> return m

-- | Print game errors
printErr :: GameError -> Game ()
printErr err =
  lift . lift . print $
  case err of
    MoveParseError -> "You entered a malformed move"
    InvalidMove -> "Your move was incorrect"

prompt :: TurnState -> String
prompt PlacingRed = "Place a red piece"
prompt PlacingWhite = "Place a white piece"
prompt PlacingBlack = "Place a black piece"
prompt MoveBlack = "Black Move"
prompt MoveWhite = "White Move"
prompt ts = show ts

-- | the takeTurn computation represents a player inputting a well-formed, valid
-- move and it being executed on the dvonn board.
takeTurn :: Game ()
takeTurn = do
  m <- getTurnInput
  executeMove m

-- | Phase 1 is a deterministic sequence of placements. We produce the ordered
-- list of which piece must be placed and and ask for a turn to be taken for
-- each placement
phase1 :: Game ()
phase1 = sequence_ $ intersperse takeTurn phase1Turns
  where
    updateTurn :: TurnState -> Game ()
    updateTurn t = modify $ \gs -> gs {turn = t}

    phase1Turns = updateTurn <$>
      [PlacingRed, PlacingRed, PlacingRed] ++
      take 46 (cycle [PlacingBlack, PlacingWhite])

-- | Phase 2 is less deterministic. It consists of recursively taking turns
-- until the end state of the game is reached, at which point the winner is
-- determined (if there is one)
phase2 :: Game (Maybe Player)
phase2 = do
  takeTurn
  (GameState b t _) <- get
  case t of
    End -> return $ calcWinner b
    _ -> phase2

-- | Dvonn represents the entire game as a computation
dvonn :: Game (Maybe Player)
dvonn = rules >> phase1 >> modify (\gs -> gs {phase = Phase2, turn = MoveBlack}) >> phase2

-- | A function that lets us evaluate the Game monad
evalGame :: GameState -> Game a -> IO (Either GameError (a, GameState))
evalGame s g = runExceptT (runStateT g s)

rules :: Game ()
rules = liftGame $ putStrLn (summary1 ++ summary2 ++ "\n")

-- | Determine which player should play from the turnstate
turn2player :: TurnState -> Player
turn2player t
  | t `elem` [PlacingWhite,MoveWhite] = PWhite
  | t `elem` [PlacingBlack,MoveBlack] = PBlack
  | otherwise = PWhite

summary1 :: String
summary1 =
  "Welcome to DVONN." ++
  "\n" ++
  "\n" ++
  "In Phase 1, you must place your pieces one by one on" ++
  "\n" ++
  "unoccupied spaces on the game board. White will begin" ++
  "\n" ++
  "and players will alternate placing. Place your red" ++
  "\n" ++
  "pieces first, and then proceed to place your normal pieces." ++
  "\n" ++
  "White has two red pieces and Black has one red piece." ++
  "\n" ++
  "\n" ++
  "To place a piece, use the command <coord>, for example, 'A3'." ++
  "\n"

summary2 :: String
summary2 =
  "In Phase 2, you may move any stack of height n with your color" ++
  "\n" ++
  "piece on top to be on top of another stack that is exactly" ++
  "\n" ++
  "n hops away. Be careful, though! Any stacks that are not" ++
  "\n" ++
  "in a connected component with a red piece will be discarded." ++
  "\n" ++
  "Whoever controls the most pieces at the end wins!." ++
  "\n" ++
  "\n" ++
  "To jump, use the command <start> 'to' <end>, for example 'A1 to A3'." ++
  "\n"
