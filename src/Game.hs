module Game where

import Control.Monad.State (get, StateT, gets, modify, lift)
import Control.Monad.Except (liftEither, throwError, ExceptT, lift)
import Control.Monad

import Board (emptyBoard, Board)
import Defs
import Move

liftGame :: IO a -> Game a
liftGame = Control.Monad.State.lift . Control.Monad.Except.lift

type Game = StateT GameState (ExceptT GameError IO)

apply:: Move -> GameState -> GameState
apply = undefined

executeMove :: Move -> Game ()
executeMove m = do
  gs <- get
  if validMove gs m
    then modify (apply m)
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


takeTurn :: Game ()
takeTurn = do
  m <- getTurnInput
  executeMove m

