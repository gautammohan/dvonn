-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Move where

import Data.Map as M
import Data.List
import Board

import Defs

-- | Only linear moves are permitted
isLinear :: Move -> Bool
isLinear m = case end m - start m of
   Coordinate (0, _) -> True
   Coordinate (_, 0) -> True
   Coordinate (x,y) -> x == y

-- | As of now, we cannot move a stack to a place off the board.
-- TODO: Adjust this when handling discards?
isOnBoard :: Move -> Bool
isOnBoard m = validCoordinate (start m) && validCoordinate (end m)

-- | Calculates height of stack on a coordinate, which controls
-- how many spaces that stack must be moved.
distance :: Move -> Int
distance m = let Coordinate (x,y) = end m - start m in
    max (abs x) (abs y)

-- | Determines which player owns a stack based on which color
-- piece is on top
playerOwns :: Player -> Stack -> Bool
playerOwns p (Stack s) = case p of
   PBlack -> head s == Black
   PWhite -> head s == White

-- | Checks that the proposed move satisfies the following conditions
--  1. The start and ending spaces are on the board and both contain stacks
--  2. The move is linear
--  3. The starting coordinate is not surrounded
--  4. The move is the correct distance
--  5. The player owns the stack he or she proposes to move
validMove :: Board -> Move -> Bool
validMove b m = case M.lookup (start m) (getMap b) of
   Just (Stack s)  -> isOnBoard m && isLinear m && 
              not (isSurrounded (start m) b) &&
              distance m == length s && playerOwns (player m) (Stack s)
   _       -> False

-- | Places a piece on an empty coordinate in the first phase of the game
--
--TODO: I'm not sure how to fill this in with the Game Monad. This is something
--we will probably want to replace later.
--Figure out how to check if this placement is valid,
--probably in the game monad.
placePiece :: Board -> Piece -> Coordinate -> Board
placePiece b p c = Board $ M.insert c (Stack [p]) (getMap b)

-- | Finds all possible moves based on the board state
getPossibleMoves :: Board -> [Move]
getPossibleMoves = undefined

-- TODO: What is the type signature here? Move to an IO module?
-- Need to add something for displaying the board state
parseMove = undefined
