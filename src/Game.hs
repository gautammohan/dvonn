module Game where

import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import Control.Monad

import Board (Board)

data TurnState
  = PlacingRed
  | PlacingWhite
  | PlacingBlack
  | MoveWhite
  | MoveBlack
  | Start
  | End

data GameError = ParseError

type GameState = (Board, TurnState)

type Game = StateT GameState (ExceptT GameError IO)
