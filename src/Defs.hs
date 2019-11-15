module Defs where

data Board = Board

newtype Coordinate =
  Coordinate (Int, Int)

data TurnState
  = PlacingRed
  | PlacingWhite
  | PlacingBlack
  | MoveWhite
  | MoveBlack
  | Start
  | End

data GamePhase = Phase1 | Phase2

data Piece
  = Red
  | White
  | Black

data Player
  = PBlack
  | PWhite deriving (Show)


data Move = Move
  { player :: Player
  , start :: Coordinate
  , end :: Coordinate
  }

data GameState = GameState
  { board :: Board
  , turn :: TurnState
  , phase :: GamePhase
  }

data GameError = InvalidMove | ParseError deriving (Show)
