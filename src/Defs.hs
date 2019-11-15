module Defs where

import Board (Board, Coordinate)

data TurnState
  = PlacingRed
  | PlacingWhite
  | PlacingBlack
  | MoveWhite
  | MoveBlack
  | Start
  | End

data Piece
  = Red
  | White
  | Black

data Player
  = PBlack
  | PWhite


data Move = Move
  { player :: Player
  , start :: Coordinate
  , end :: Coordinate
  }

data GameState = GameState
  { board :: Board
  , turn :: TurnState
  }

data GameError = InvalidMove deriving (Show)
