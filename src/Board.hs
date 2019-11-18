module Board where

import Data.Map as M hiding (null, foldr, filter)
import Data.List

-- Is it worth having sign?
-- More general version of board
-- data Board = Board {getMap :: M.Map Coordinate Stack, width :: Int, height :: Int}

import Defs

emptyBoard :: Board
emptyBoard = undefined

validCoordinate :: Coordinate -> Bool
validCoordinate c = c `elem` [Coordinate (x,y) | x <- [3 .. 9], y <- [1 .. 5]]
                 || c `elem` [Coordinate (x,y) | x <- [2,10], y <- [1 ..4]]
                 || c `elem` [Coordinate (x,y) | x <- [1,11], y <- [1 .. 3]]

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

calcWinner = undefined
apply = undefined
getNextTurn = undefined

-- TODO TEST THIS
--
nonempty :: Board -> [Coordinate]
nonempty = undefined

-- count empty spaces on the board
countEmpty :: Board -> Int
countEmpty = undefined
