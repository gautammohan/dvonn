module Game where

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

import Board (Board, emptyBoard)
import Defs
import Move

liftGame :: IO a -> Game a
liftGame = Control.Monad.State.lift . Control.Monad.Except.lift

type Game = StateT GameState (ExceptT GameError IO)

apply:: Move -> GameState -> GameState
apply = undefined

getNextTurn :: Move -> GameState -> TurnState
getNextTurn = undefined

executeMove :: Move -> Game ()
executeMove m = do
  gs <- get
  if validMove gs m
    then do
      let t = getNextTurn m gs
      modify (apply m) >> modify (\gs -> gs {turn = t})
    else throwError InvalidMove

gameStart :: GameState
gameStart = GameState emptyBoard Start

-- game begins by players
phase1 :: [TurnState]
phase1 =
  [PlacingRed, PlacingRed, PlacingRed] ++
  take 46 (cycle [PlacingBlack, PlacingWhite])

getTurnInput :: Game Move
getTurnInput = do
  s <- (lift . lift) getLine
  liftEither $ parseMove s

printErr :: GameError -> Game ()
printErr = lift . lift . print

takeTurn :: Game ()
takeTurn = do
  m <- getTurnInput `catchError` (\e -> printErr e >> getTurnInput)
  executeMove m

runGame :: Game ()
runGame = do
  takeTurn
  (GameState b t) <- get
  case t of
    End -> (lift . lift) (putStrLn $ winnerStr (calcWinner b))
    _ -> takeTurn

calcWinner :: Board -> Player
calcWinner b = undefined

winnerStr :: Player -> String
winnerStr _ = "hello"
