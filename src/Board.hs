-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Board where

import Data.Map as M hiding (null, foldr, filter, map)
import Data.Monoid
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
emptyBoard =
  Board
    { getDiscard = Stack []
    , getMap = foldr (\x acc -> M.insert x (Stack []) acc) M.empty l
    }
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

-- | Determines if a coordinate has a stack with a red piece
containsRed :: Board -> Coordinate -> Bool
containsRed b c  = case M.lookup c (getMap b) of
       Just (Stack s) -> Red `elem` s
       _  -> False

hasRed :: Board -> S.Set Coordinate -> Bool
hasRed b cs = not . S.null $ S.map (containsRed b) cs

-- | Constructs the list of all active coordinates (i.e., coordinates
-- with stacks) that are neighbors of the given coordinate
neighbors :: Coordinate -> Board -> S.Set Coordinate
neighbors c b = S.filter (nonempty b) (allNeighbors c)

-- | Find the component of nonempty spaces that a coordinate belongs to given a
-- board.
component :: Board -> Coordinate -> S.Set Coordinate
component b c = aux b (S.fromList [c]) (neighbors c b)
    -- Mutually recursive functions perform DFS to find all neighboring
    -- coordinates of a component given unseen coordinates
  where
    aux :: Board -> S.Set Coordinate -> S.Set Coordinate -> S.Set Coordinate
    aux b component ns =
      let frontier = ns S.\\ component
       in S.foldl' addNewNeighbors component frontier

    addNewNeighbors :: S.Set Coordinate -> Coordinate -> S.Set Coordinate
    addNewNeighbors curr newCoord =
      let updated = newCoord `S.insert` curr
       in aux b updated (neighbors newCoord b)

allComponents :: Board -> [S.Set Coordinate]
allComponents b = aux [] coordinates
  where
    -- recursively find coordinates not in any component we have seen and add
    -- its component to the list
    aux :: [S.Set Coordinate] -> S.Set Coordinate -> [S.Set Coordinate]
    aux l coords =
      let unseen = coords S.\\ mconcat l
       in if S.null unseen
            then l
            else let (c, cs) = S.deleteFindMin unseen
                  in aux (component b c : l) cs

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

calcWinner :: Board -> Maybe Player
calcWinner b = do
    let remaining = S.map (innerstack b) (nonempties b)
        whiteStacks = S.filter (\s -> head s == White) remaining
        blackStacks = S.filter (\s -> head s == Black) remaining
        white = concat whiteStacks
        black = concat blackStacks
    if length white > length black then Just PWhite
    else if length black > length white then Just PBlack
    else Nothing

-- | Executes a move
-- TODO: Error handling
apply :: Move -> Board -> Board
apply = undefined

combine :: Board -> Coordinate -> Coordinate -> Board
combine b c1 c2 =
  let newStack = Stack $ innerstack b c1 ++ innerstack b c2
      newM = M.insert c2 (Stack []) (M.insert c1 newStack (getMap b))
   in b {getMap = newM}

discard :: Board -> S.Set Coordinate -> Board
discard b cs =
  let m = getMap b
      discards = Stack $ S.foldr (\c s -> getStack (m M.! c) ++ s) [] cs
      newM = S.foldr (\c m -> M.insert c (Stack []) m) m cs
   in b {getMap = newM, getDiscard = discards}

cleanup :: Board -> Board
cleanup b =
  let emptyComponents = filter (hasRed b) (allComponents b)
   in foldr (flip discard) b emptyComponents

innerstack :: Board -> Coordinate -> [Piece]
innerstack b = getStack . (getMap b M.!)

place :: Board -> Piece -> Coordinate -> Board
place b p c =
  let newB = M.adjust (addToStack p) c (getMap b)
      addToStack p (Stack s) = Stack (p : s)
   in b {getMap = newB}

-- | Determines which player goes next
getNextTurn :: Move -> Board -> TurnState -> TurnState
getNextTurn = undefined

-- | Returns list of nonempty spaces on board,  not including discard pile
--TODO Should we include discard pile here???
nonempties :: Board -> S.Set Coordinate
nonempties b = S.filter (nonempty b) coordinates

-- | Check if a coordinate on the board is nonempty
nonempty :: Board -> Coordinate -> Bool
nonempty b = not . null . innerstack b

-- | Counts empty spaces on the board, not including the discard pile
countEmpty :: Board -> Int
countEmpty = length . nonempties
