module Defs where

import Data.Map as M

-- Maybe change coordinate to record syntax
newtype Coordinate = Coordinate (Int, Int)
    deriving (Show, Eq, Ord)

newtype Board = Board {getMap :: M.Map Coordinate Stack}
    deriving (Show)

data Piece = Red | White | Black
    deriving (Show, Eq)

newtype Stack = Stack [Piece]
    deriving (Show)

instance Num Coordinate where
   (+) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1+x2, y1+y2)
   (-) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1-x2, y1-y2)
   (*) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1*x2, y1*y2)
   fromInteger n = Coordinate (fromInteger n, fromInteger n)
   negate (Coordinate (x1, y1)) = Coordinate (-x1, -y1)
   abs (Coordinate (x1, y1)) = abs (Coordinate (abs x1, abs y1))
   signum (Coordinate (x1, y1)) = Coordinate (signum x1, signum y1)
--this is weird, do we have to derive everything?
--TODO is it safe to derive - here?

data TurnState
  = PlacingRed
  | PlacingWhite
  | PlacingBlack
  | MoveWhite
  | MoveBlack
  | Start
  | End

data GamePhase = Phase1 | Phase2

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
