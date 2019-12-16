-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Defs where

import qualified Data.Map as M
import qualified Data.Set as S

-- | Dvonn has a 2D board, so a coordinate is a pair of Ints
newtype Coordinate = Coordinate (Int, Int)
    deriving (Show, Eq, Ord, Read)

-- | A connected component is a set of unique coordinates
type Component = S.Set Coordinate

-- | A board is a mapping of valid coordinates to their contents, stacks of
-- pieces, and a discard pile.
data Board = Board {getMap :: M.Map Coordinate Stack, getDiscard :: Stack}
    deriving (Show, Eq, Read)

-- | Adds all game coordinates, to a map and initializes each value to an empty
-- stack
emptyBoard :: [Coordinate] -> Board
emptyBoard coordinates =
  Board
    { getDiscard = Stack []
    , getMap = foldr (\x acc -> M.insert x (Stack []) acc) M.empty coordinates
    }

-- | Initializes an empty dvonn board with all valid coordinates
emptyDvonn :: Board
emptyDvonn =
  emptyBoard $
  [Coordinate (x, y) | x <- [3 .. 9], y <- [1 .. 5]] ++
  [Coordinate (x, y) | x <- [2], y <- [1 .. 4]] ++
  [Coordinate (x, y) | x <- [1], y <- [1 .. 3]] ++
  [Coordinate (x, y) | x <- [10], y <- [2 .. 5]] ++
  [Coordinate (x, y) | x <- [11], y <- [3 .. 5]]

-- | Initializes an empty mini board (for testing)
emptyMini :: Board
emptyMini = emptyBoard [Coordinate (x, y) | x <- [1 .. 3], y <- [1 .. 3]]

-- | Peces can be red, white, or black
data Piece = Red | White | Black
    deriving (Show, Eq, Ord, Read)

-- | A Stack is an ordered list of pieces. The head of the list represents the topmost stack
newtype Stack = Stack
  { getStack :: [Piece]
  } deriving (Show, Eq, Read)

instance Num Coordinate where
   (+) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1+x2, y1+y2)
   (-) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1-x2, y1-y2)
   (*) (Coordinate (x1,y1)) (Coordinate (x2,y2)) = Coordinate (x1*x2, y1*y2)
   fromInteger n = Coordinate (fromInteger n, fromInteger n)
   negate (Coordinate (x1, y1)) = Coordinate (-x1, -y1)
   abs (Coordinate (x1, y1)) = abs (Coordinate (abs x1, abs y1))
   signum (Coordinate (x1, y1)) = Coordinate (signum x1, signum y1)
-- TODO, do we have to derive everything, and is it safe?

-- | In Phase 1, players can be in one of 3 Placing phases. Phase 2 alternates
-- between black and white making moves
data TurnState
  = PlacingRed
  | PlacingWhite
  | PlacingBlack
  | MoveWhite
  | MoveBlack
  | Start
  | End deriving (Show, Eq)

-- | Phase 1 is where the players fill the board with pieces, and Phase 2 is where they make jumps
data GamePhase = Phase1 | Phase2 deriving (Show, Read)

-- | Dvonn has two players, black and white
data Player
  = PBlack
  | PWhite deriving (Show, Eq, Read)


-- | A move is either a jump, where the stack at the start coordinate is placed
-- on top of the stack at the end coordinate. A Place move places a piece on a
-- coordinate
data Move
  = Jump { player :: Player
         , start :: Coordinate
         , end :: Coordinate }
  | Place { color :: Piece
          , loc :: Coordinate }
  deriving (Show, Eq)

-- | A GameState holds all the information necessary to determine the state of
-- the game. The board holds the active stacks and discarded pieces. The turn
-- tells us the current player's turn, and the phase lets us know whether we
-- should accept Jump or Place moves
data GameState = GameState
  { board :: Board
  , turn :: TurnState
  , phase :: GamePhase
  }

-- | An extensible enumeration of different game errors that can occur during
-- play
data GameError = InvalidMove | MoveParseError deriving (Show, Eq)
