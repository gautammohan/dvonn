-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Board where

import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Defs
import Data.List
import Data.Maybe


diff :: Board -> Board -> (M.Map Coordinate (Maybe (Stack, Stack)),Maybe (Stack, Stack))
diff (Board m1 d1) (Board m2 d2) =
  (M.filter isJust (M.intersectionWith f m1 m2), discardDiff)
  where
    f s1 s2 =
      if s1 == s2
        then Nothing
        else Just (s1, s2)
    discardDiff =
      if d1 == d2
        then Nothing
        else Just (d1, d2)


coordinates :: Board -> S.Set Coordinate
coordinates = S.fromList . M.keys . getMap

-- | Determines if a coordinate is on the board
validCoordinate :: Board -> Coordinate -> Bool
validCoordinate b = (`elem` coordinates b)

validDvonnCoordinate :: Coordinate -> Bool
validDvonnCoordinate = validCoordinate emptyDvonn
validMiniCoordinate :: Coordinate -> Bool
validMiniCoordinate = validCoordinate emptyMini

allNeighbors :: Board -> Coordinate -> S.Set Coordinate
allNeighbors b (Coordinate (x, y)) =
  S.fromList $
  filter (validCoordinate b) $
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
neighborOf :: Board -> Coordinate -> Coordinate -> Bool
neighborOf b c1 c2 = validCoordinate b c2 && c1 `S.member` allNeighbors b c2

-- | Determines if a coordinate has a stack with a red piece
containsRed :: Board -> Coordinate -> Bool
containsRed b c  = case M.lookup c (getMap b) of
       Just (Stack s) -> Red `elem` s
       _  -> False

-- | Determines if there is a red piece in a set of coordinates
hasRed :: Board -> S.Set Coordinate -> Bool
hasRed b cs = S.member True $ S.map (containsRed b) cs

-- | Constructs the list of all active coordinates (i.e., coordinates
-- with stacks) that are neighbors of the given coordinate
neighbors :: Coordinate -> Board -> S.Set Coordinate
neighbors c b = S.filter (nonempty b) (allNeighbors b c)

-- | Find the component of nonempty spaces that a coordinate belongs to given a
-- board.
component :: Board -> Coordinate -> S.Set Coordinate
component b c =
  if nonempty b c
    then aux b (S.fromList [c]) (neighbors c b)
    else S.empty
  where
    -- Mutually recursive functions perform DFS to find all neighboring
    -- coordinates of a component given unseen coordinates
    aux :: Board -> S.Set Coordinate -> S.Set Coordinate -> S.Set Coordinate
    aux b component ns =
      let frontier = ns S.\\ component
       in S.foldl' addNewNeighbors component frontier
    addNewNeighbors :: S.Set Coordinate -> Coordinate -> S.Set Coordinate
    addNewNeighbors curr newCoord =
      let updated = newCoord `S.insert` curr
       in aux b updated (neighbors newCoord b)

allComponents :: Board -> [S.Set Coordinate]
allComponents b = aux [] (nonempties b)
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
isSurrounded :: Board -> Coordinate -> Bool
isSurrounded b c = length (neighbors c b) == 6

calcWinner :: Board -> Maybe Player
calcWinner b = do
    let remaining = Data.List.map (innerstack b) (S.toList (nonempties b))
        whiteStacks = filter (\s -> head s == White) remaining
        blackStacks = filter (\s -> head s == Black) remaining
        white = concat whiteStacks
        black = concat blackStacks
    if length white > length black then Just PWhite
    else if length black > length white then Just PBlack
    else Nothing

-- | Executes a move
-- TODO: Error handling
apply :: Move -> Board -> Board
apply (Move _ c1 c2) b =
  let b' = combine b c1 c2
      b'' = cleanup b'
   in b''

combine :: Board -> Coordinate -> Coordinate -> Board
combine b c1 c2 =
  let newStack = Stack $ innerstack b c1 ++ innerstack b c2
      newM = M.insert c1 (Stack []) (M.insert c2 newStack (getMap b))
   in b {getMap = newM}

discard :: Board -> S.Set Coordinate -> Board
discard b cs =
  let m = getMap b
      discards = Stack $ S.foldr (\c s -> getStack (m M.! c) ++ s) [] cs
      newM = S.foldr (\c m -> M.insert c (Stack []) m) m cs
   in b {getMap = newM, getDiscard = discards}

cleanup :: Board -> Board
cleanup b =
  let emptyComponents = filter (not . hasRed b) (allComponents b)
   in foldr (flip discard) b emptyComponents

innerstack :: Board -> Coordinate -> [Piece]
innerstack b = getStack . (getMap b M.!)

place :: Board -> Piece -> Coordinate -> Board
place b p c =
  let newB = M.adjust (addToStack p) c (getMap b)
      addToStack p (Stack s) = Stack (p : s)
   in b {getMap = newB}

-- | Returns list of nonempty spaces on board,  not including discard pile
nonempties :: Board -> S.Set Coordinate
nonempties b = S.filter (nonempty b) (coordinates b)

empties :: Board -> S.Set Coordinate
empties b = S.filter (not . nonempty b) (coordinates b)

-- | Check if a coordinate on the board is nonempty
nonempty :: Board -> Coordinate -> Bool
nonempty b = not . null . innerstack b

-- | Counts empty spaces on the board, not including the discard pile
countEmpty :: Board -> Int
countEmpty = S.size . empties

-- | Count the number of active pieces on the board (summing each stack)
numActivePieces :: Board -> Int
numActivePieces =
  getSum . foldMap (Sum . length . getStack . snd) . M.toList . getMap

numDiscardedPieces :: Board -> Int
numDiscardedPieces = length . getStack . getDiscard
