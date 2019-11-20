-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Board where

import Data.Map as M hiding (null, foldr, filter, map)
import Data.List
import Defs

emptyBoard :: Board
emptyBoard = Board $ foldr (\x acc -> M.insert x (Stack []) acc) M.empty l where
    l = Coordinate (-1,-1) : [Coordinate (x,y) | x <- [1 ..11], y <- [1..5],
                                             validCoordinate (Coordinate (x,y))]

validCoordinate :: Coordinate -> Bool
validCoordinate c = c `elem` [Coordinate (x,y) | x <- [3 .. 9], y <- [1 .. 5]]
                 || c `elem` [Coordinate (x,y) | x <- [2,10], y <- [1 ..4]]
                 || c `elem` [Coordinate (x,y) | x <- [1,11], y <- [1 .. 3]]

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

containsRed :: Board -> Coordinate -> Bool
containsRed b c  = case M.lookup c (getMap b) of
       Just (Stack s) -> Red `elem` s
       _  -> False

neighbors :: Coordinate -> Board -> [Coordinate]
neighbors c b = [Coordinate (x,y) | x <- [1 ..11], y<- [1..5],
                                    M.member (Coordinate (x,y)) (getMap b),
                                    validCoordinate (Coordinate (x,y)),
                                    areNeighbors c (Coordinate (x,y))]


--This may be very inefficient with the exhaustive neighbor search
connected :: Coordinate -> Board -> Bool
connected c b =  containsRed b c || connectedHelper [c] b [] where
       connectedHelper xs b visited = do           
           let frontier = Data.List.nub $ foldr
                  (\y ys -> neighbors y b ++ ys) ([] :: [Coordinate]) xs
               frontier' = filter (`notElem` visited) frontier
           not (null frontier') &&
             (do let visited' = visited' ++ frontier'
                 any (containsRed b) frontier' || connectedHelper frontier' b visited')

calcWinner :: Board -> Player
calcWinner = undefined

--calcWinner b = do
--   let stacks = map (\(Just y) -> y) $ map (\x -> M.lookup x (getMap b)) (nonempty b) 
--       whiteStacks = filter (\(Stack s) -> head s == Black) stacks 
--       blackStacks = filter (\(Stack s) -> head s == White) stacks
--   if length whiteStacks > length blackStacks then PWhite else PBlack
      
--nonEmptyStacks = filter (\x -> case x of
--                                    Nothing -> False
--                                    _ -> True) stacks

apply :: Move -> Board -> Board
apply = undefined

getNextTurn :: Move -> Board -> TurnState -> TurnState 
getNextTurn = undefined

--TODO Should we include discard pile here???
-- returns list of nonempty spaces on board,  not including discard pile
nonempty :: Board -> [Coordinate]
nonempty b = filter (/= Coordinate (-1,-1)) $ map fst (filter (\(x, Stack y) 
                     -> (not $ null y)) (M.toList (getMap b)))

-- count empty spaces on the board
countEmpty :: Board -> Int
countEmpty b = length $ filter (\(x,y) -> x /= Coordinate (-1,-1)) 
                    (filter (\(x, Stack y) -> null y) (M.toList (getMap b))) 
