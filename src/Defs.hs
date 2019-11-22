-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Defs where

import Data.Map as M
import Data.Set as S

newtype Coordinate = Coordinate (Int, Int)
    deriving (Show, Eq, Ord)

newtype Path = Path {getPath :: [Coordinate]}

type Component = S.Set Coordinate

newtype Board = Board {getMap :: M.Map Coordinate Stack}
    deriving (Show)

data Piece = Red | White | Black
    deriving (Show, Eq)

newtype Stack = Stack
  { getStack :: [Piece]
  } deriving (Show)

instance Num Coordinate where
   (+) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1+x2, y1+y2)
   (-) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1-x2, y1-y2)
   (*) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1*x2, y1*y2)
   fromInteger n = Coordinate (fromInteger n, fromInteger n)
   negate (Coordinate (x1, y1)) = Coordinate (-x1, -y1)
   abs (Coordinate (x1, y1)) = abs (Coordinate (abs x1, abs y1))
   signum (Coordinate (x1, y1)) = Coordinate (signum x1, signum y1)
-- TODO, do we have to derive everything, and is it safe?

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
