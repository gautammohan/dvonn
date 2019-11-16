module Board where

import Data.Map as M hiding (null, foldr, filter)
import Data.List

-- Is it worth having sign?
-- More general version of board
-- data Board = Board {getMap :: M.Map Coordinate Stack, width :: Int, height :: Int}

data Board = Board {getMap :: M.Map Coordinate Stack}

-- Maybe change coordinate to record syntax
newtype Coordinate = Coordinate (Int, Int)
    deriving (Show, Eq, Ord)

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
          0 -> if abs (y2 - y1) == 1 then True else False
          1 -> case (abs (y2 - y1)) of
              0 -> True
              1 -> if  (x2 - x1) + (y2 - y1) == 0 then False else True  -- Cute
              _ -> False
          _ -> False

containsRed :: Board -> Coordinate -> Bool
containsRed b c  = case M.lookup c (getMap b) of
       Just (Stack s) -> Red `elem` s
       _  -> False
                   
neighbors :: Coordinate -> Board -> [Coordinate]
neighbors c b = [(Coordinate (x,y)) | x <- [1 ..9], y<- [1..5],
                                                M.member (Coordinate (x,y)) (getMap b),
                                                validCoordinate (Coordinate (x,y)),
                                                areNeighbors c (Coordinate (x,y))]

connected :: Coordinate -> Board -> Bool
connected c b = do 
   let visited = []
   connectedHelper [c] b visited where
       connectedHelper xs b v = do           --- This is very inefficient, maybe just make list of neighbors
           let frontier = Data.List.nub $ foldr (\y ys -> (neighbors y b) ++ ys) ([] :: [Coordinate]) xs
               frontier' = filter (\x -> notElem x v) frontier
           if null frontier' then False else do
                let visited = visited ++ frontier'
                if any (containsRed b) frontier' then True else connectedHelper frontier' b visited 

-- TODO TEST THIS
