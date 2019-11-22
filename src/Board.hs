-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Board where

import Data.Map as M hiding (null, foldr, filter, map)
import qualified Data.Set as S
import Defs
import Data.List


coordinates :: S.Set Coordinate
coordinates = S.fromList $
  [Coordinate (x, y) | x <- [3 .. 9], y <- [1 .. 5]] ++
  [Coordinate (x, y) | x <- [2, 10], y <- [1 .. 4]] ++
  [Coordinate (x, y) | x <- [1, 11], y <- [1 .. 3]]

-- | Adds all game coordinates, to a map and initializes each value to an empty
-- stack
emptyBoard :: Board
emptyBoard = Board $ foldr (\x acc -> M.insert x (Stack []) acc) M.empty l
  where
    l = S.toList coordinates

-- | Determines if a coordinate is on the board, excluding the discard pile
validCoordinate :: Coordinate -> Bool
validCoordinate c = c `elem` coordinates

allNeighbors :: Coordinate -> S.Set Coordinate
allNeighbors (Coordinate (x, y)) =
  S.fromList $
  filter validCoordinate $
  map
    Coordinate
    [ (x + 1, y)
    , (x - 1, y)
    , (x, y + 1)
    , (x, y - 1)
    , (x + 1, y + 1)
    , (x - 1, y - 1)
    ]

-- | c1 `neighborOf` c2 implies that c1 and c2 are neighbors since this relation
-- is reflexive
neighborOf :: Coordinate -> Coordinate -> Bool
neighborOf c1 c2 = c1 `S.member` allNeighbors c2


-- | Determines if two coordinates are neighbors.
areNeighbors :: Coordinate -> Coordinate -> Bool
areNeighbors (Coordinate (x1,y1)) (Coordinate (x2, y2))
      = validCoordinate (Coordinate (x1,y1)) &&
        validCoordinate (Coordinate (x2, y2)) &&
        case abs (x2 - x1) of
          0 -> abs (y2 - y1) == 1
          1 -> case abs (y2 - y1) of
              0 -> True
              1 -> ((x2 - x1) + (y2 - y1)) /= 0
              _ -> False
          _ -> False

-- | Determines if a coordinate has a stack with a red piece
containsRed :: Board -> Coordinate -> Bool
containsRed b c  = case M.lookup c (getMap b) of
       Just (Stack s) -> Red `elem` s
       _  -> False

-- | Constructs the list of all active coordinates (i.e., coordinates
-- with stacks) that are neighbors of the given coordinate
neighbors :: Coordinate -> Board -> S.Set Coordinate
neighbors c b = S.filter (nonempty b) (allNeighbors c)
-- neighbors c b = [Coordinate (x,y) | x <- [1 ..11], y<- [1..5],
--                                     M.member (Coordinate (x,y)) (getMap b),
--                                     validCoordinate (Coordinate (x,y)),
--                                     areNeighbors c (Coordinate (x,y))]

--UNTESTED
component :: Board -> Coordinate -> S.Set Coordinate
component b c = aux b (S.fromList [c]) (neighbors c b)
  where
    aux :: Board -> S.Set Coordinate -> S.Set Coordinate -> S.Set Coordinate
    aux b component ns =
      if S.null frontier
        then component
        else S.foldl'
               (\base elt ->
                  (elt `S.insert` base) `S.union`
                  aux b base (neighbors (elt :: Coordinate) b))
               component
               frontier
      where
        frontier = ns S.\\ component

-- | Checks to see whether a coordinate has six neighbors. If so, it is
-- not permitted to be moved.
isSurrounded :: Coordinate -> Board -> Bool
isSurrounded c b = length (neighbors c b) == 6

-- | Starting at coordinate c, determines whether there is a path through
-- active coordinates to a coordinate containing a red piece. In other
-- words, is coordinate c in a connected component with a red piece?
-- TODO : This may be very inefficient with the exhaustive neighbor search
--       and need to test
-- connected :: Coordinate -> Board -> Bool
-- connected c b =  containsRed b c || connectedHelper [c] b [] where
--        connectedHelper xs b visited = do
--            let frontier = Data.List.nub $ foldr
--                   (\y ys -> neighbors y b ++ ys) ([] :: [Coordinate]) xs
--                frontier' = filter (`notElem` visited) frontier
--            not (null frontier') &&
--              (do let visited' = visited' ++ frontier'
--                  any (containsRed b) frontier' || connectedHelper frontier' b visited')

-- | Determines which player has won based on who has the most pieces overall
-- from stacks that that player controls.
-- TODO: Is it possible to have a tie?
calcWinner :: Board -> Player
calcWinner = undefined

--calcWinner b = do
--   let stacks = map (\(Just y) -> y) $ map (\x -> M.lookup x (getMap b)) (nonempty b)
--       whiteStacks = filter (\(Stack s) -> head s == Black) stacks
--       blackStacks = filter (\(Stack s) -> head s == White) stacks
--   if length whiteStacks > length blackStacks then PWhite else PBlack
-- nonEmptyStacks = filter (\x -> case x of
--                                    Nothing -> False
--                                    _ -> True) stacks

-- | Executes a move
-- TODO: Error handling
apply :: Move -> Board -> Board
apply = undefined

-- | Determines which player goes next
getNextTurn :: Move -> Board -> TurnState -> TurnState
getNextTurn = undefined

-- | Returns list of nonempty spaces on board,  not including discard pile
--TODO Should we include discard pile here???
nonempties :: Board -> S.Set Coordinate
nonempties b = S.filter (nonempty b) coordinates

-- | Check if a coordinate on the board is nonempty
nonempty :: Board -> Coordinate -> Bool
nonempty b = not . null . getStack . (getMap b M.!)

-- | Counts empty spaces on the board, not including the discard pile
countEmpty :: Board -> Int
countEmpty = length . nonempties
