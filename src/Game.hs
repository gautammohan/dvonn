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

liftGame :: IO a -> Game a
liftGame = Control.Monad.State.lift . Control.Monad.Except.lift

type Game = StateT GameState (ExceptT GameError IO)

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

startState :: GameState
startState = GameState emptyDvonn Start Phase1

getTurnInput :: Game Move
getTurnInput = do
  b <- gets board
  t <- gets turn
  liftGame $ printBoard b
  liftGame $ putStr $ show t ++ "> "
  s <- liftGame getLine
  case liftEither $ parseMove s t of
    Left e -> printErr e >> getTurnInput
    Right m -> return m

printErr :: GameError -> Game ()
printErr = lift . lift . print

takeTurn :: Game ()
takeTurn = do
  m <- getTurnInput
  executeMove m

phase1 :: Game ()
phase1 = sequence_ $ intersperse takeTurn phase1Turns
  where
    updateTurn :: TurnState -> Game ()
    updateTurn t = modify $ \gs -> gs {turn = t}

    phase1Turns = updateTurn <$>
      [PlacingRed, PlacingRed, PlacingRed] ++
      take 46 (cycle [PlacingBlack, PlacingWhite])

phase2 :: Game (Maybe Player)
phase2 = do
  takeTurn
  (GameState b t _) <- get
  case t of
    End -> return $ calcWinner b
    _ -> phase2

dvonn :: Game (Maybe Player)
dvonn = phase1 >> modify (\gs -> gs {phase = Phase2, turn = MoveBlack}) >> phase2

evalGame :: GameState -> Game a -> IO (Either GameError (a, GameState))
evalGame s g = runExceptT (runStateT g s)

turn2player :: TurnState -> Player
turn2player t
  | t `elem` [PlacingWhite,MoveWhite] = PWhite
  | t `elem` [PlacingBlack,MoveBlack] = PBlack
  | otherwise = PWhite
