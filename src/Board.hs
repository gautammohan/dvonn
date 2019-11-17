module Board where

import Data.Map as M hiding (null, foldr, filter)
import Data.List

-- Is it worth having sign?
-- More general version of board
-- data Board = Board {getMap :: M.Map Coordinate Stack, width :: Int, height :: Int}

newtype Board = Board {getMap :: M.Map Coordinate Stack}

-- Maybe change coordinate to record syntax
newtype Coordinate = Coordinate (Int, Int)
    deriving (Show, Eq, Ord)

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

data Piece = Red | White | Black
    deriving (Show, Eq)

newtype Stack = Stack [Piece]

validCoordinate :: Coordinate -> Bool
validCoordinate c = c `elem` [Coordinate (x,y) | x <- [3 .. 9], y <- [1 .. 5]] 
                 || c `elem` [Coordinate (x,y) | x <- [2,6], y <- [1 ..4]] 
                 || c `elem` [Coordinate (x,y) | x <- [1,7], y <- [1 .. 3]]

areNeighbors :: Coordinate -> Coordinate -> Bool
areNeighbors (Coordinate (x1,y1)) (Coordinate (x2, y2)) 
      = case abs (x2 - x1) of
          0 -> abs (y2 - y1) == 1
          1 -> case abs (y2 - y1) of
              0 -> True
              1 -> ((x2 - x1) + (y2 - y1)) /= 0  -- Cute
              _ -> False
          _ -> False

containsRed :: Board -> Coordinate -> Bool
containsRed b c  = case M.lookup c (getMap b) of
       Just (Stack s) -> Red `elem` s
       _  -> False
                   
neighbors :: Coordinate -> Board -> [Coordinate]
neighbors c b = [Coordinate (x,y) | x <- [1 ..9], y<- [1..5],
                                    M.member (Coordinate (x,y)) (getMap b),
                                    validCoordinate (Coordinate (x,y)),
                                    areNeighbors c (Coordinate (x,y))]

connected :: Coordinate -> Board -> Bool
connected c b = do 
   let visited = []
   connectedHelper [c] b visited where
       connectedHelper xs b v = do           --- This is very inefficient, maybe just make list of neighbors
           let frontier = Data.List.nub $ foldr 
                  (\y ys -> neighbors y b ++ ys) ([] :: [Coordinate]) xs
               frontier' = filter (`notElem` v) frontier
           not (null frontier') &&
             (do let visited = visited ++ frontier'
                 any (containsRed b) frontier' || connectedHelper frontier' b visited)

-- TODO TEST THIS
--
data Player = PBlack | PWhite

data Move = Move {player :: Player, start :: Coordinate, end :: Coordinate}

--validMove first checks to see if a move is linear in the hexagonal grid

isLinear :: Move -> Bool
isLinear m = case end m - start m of
   Coordinate (0, _) -> True
   Coordinate (_, 0) -> True
   Coordinate (x,y) -> x == y 

isOnBoard :: Move -> Bool
isOnBoard m = validCoordinate (start m) && validCoordinate (end m)

distance :: Move -> Int
distance m = let Coordinate (x,y) = end m - start m in
    max (abs x) (abs y)

playerOwns :: Player -> Stack -> Bool
playerOwns p (Stack s) = case p of
   PBlack -> head s == Black
   PWhite -> head s == White

--do we add to head or tail of stack when we move???
validMove :: Board -> Move -> Bool
validMove b m = case M.lookup (start m) (getMap b) of
   Just (Stack s)  -> isOnBoard m && isLinear m && 
              distance m == length s && playerOwns (player m) (Stack s)
   _       -> False

--I'm not sure how to fill this in with the Game Monad. This is something
--we will probably want to replace later.
--TODO :: Figure out how to check if this placement is valid, 
--probably in the game monad.
placePiece :: Board -> Piece -> Coordinate -> Board
placePiece b p c = Board $ M.insert c (Stack [p]) (getMap b)

--Need to check if move is valid first
--Again, this should probably be wrapped in the Monad
executeMove :: Board -> Piece -> Coordinate -> Board
executeMove b p c =  undefined

getPossibleMoves :: Player -> Board -> [Move]
getPossibleMoves = undefined

