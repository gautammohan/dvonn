module Move where

import Data.Map as M hiding (null, foldr, filter)
import Data.List
import Board

import Defs

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

getPossibleMoves :: Player -> Board -> [Move]
getPossibleMoves = undefined

parseMove = undefined
