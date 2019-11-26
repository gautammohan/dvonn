-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Defs where

import qualified Data.Map as M
import qualified Data.Set as S

newtype Coordinate = Coordinate (Int, Int)
    deriving (Show, Eq, Ord)

type Component = S.Set Coordinate

data Board = Board {getMap :: M.Map Coordinate Stack, getDiscard :: Stack}
    deriving (Show, Eq)

-- | Adds all game coordinates, to a map and initializes each value to an empty
-- stack
emptyBoard :: [Coordinate] -> Board
emptyBoard coordinates =
  Board
    { getDiscard = Stack []
    , getMap = foldr (\x acc -> M.insert x (Stack []) acc) M.empty coordinates
    }

emptyDvonn :: Board
emptyDvonn =
  emptyBoard $
  [Coordinate (x, y) | x <- [3 .. 9], y <- [1 .. 5]] ++
  [Coordinate (x, y) | x <- [2], y <- [1 .. 4]] ++
  [Coordinate (x, y) | x <- [1], y <- [1 .. 3]] ++ 
  [Coordinate (x, y) | x <- [10], y <- [2 .. 5]] ++
  [Coordinate (x, y) | x <- [11], y <- [3 .. 5]] 

emptyMini :: Board
emptyMini = emptyBoard [Coordinate (x, y) | x <- [1 .. 3], y <- [1 .. 3]]

data Piece = Red | White | Black
    deriving (Show, Eq, Ord)

newtype Stack = Stack
  { getStack :: [Piece]
  } deriving (Show, Eq)

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
  | PWhite deriving (Show, Eq)


data Move = Move
  { player :: Player
  , start :: Coordinate
  , end :: Coordinate
  } deriving (Show, Eq)

data GameState = GameState
  { board :: Board
  , turn :: TurnState
  , phase :: GamePhase
  }

data GameError = InvalidMove | ParseError deriving (Show)
