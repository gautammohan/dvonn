-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Move (validMove, getNextTurn, parseMove, getPossibleMoves, playerOwns, isLinear, distance, isOnBoard) where

import Data.Map as M
import Data.List
import Board
import Data.Set as S
import Defs
import ParserCombinators
import Parser
import Control.Applicative
import Data.Char (ord)

-- | Only linear moves are permitted
isLinear :: Move -> Bool
isLinear m = case end m - start m of
   Coordinate (0, _) -> True
   Coordinate (_, 0) -> True
   Coordinate (x,y) -> x == y

-- | As of now, we cannot move a stack to a place off the board.
-- TODO: Adjust this when handling discards?
isOnBoard :: Board -> Move -> Bool
isOnBoard b m = validCoordinate b (start m) && validCoordinate b (end m)

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
validMove b m@Jump {} =
  case M.lookup (start m) (getMap b) of
    Just (Stack s) ->
      nonempty b (end m) &&
      isOnBoard b m &&
      isLinear m &&
      not (isSurrounded b (start m)) &&
      distance m == length s && playerOwns (player m) (Stack s)
    _ -> False
validMove b m@(Place _ coord) = coord `S.member` empties b

-- | Given the player who has just gone, if the opposing player has a possible
-- move, it is their turn. Otherwise the player goes again if there is a valid
-- move. If there are no more possible moves, the game is over
getNextTurn :: Board -> Player -> TurnState
getNextTurn b p = do
   let canMoveBlack = not $ Data.List.null [m' | m' <- getPossibleMoves b, player m' == PBlack]
       canMoveWhite = not $ Data.List.null [m' | m' <- getPossibleMoves b, player m' == PWhite]
   case p of
      PWhite | canMoveBlack -> MoveBlack
             | canMoveWhite -> MoveWhite
             | otherwise    -> End
      PBlack | canMoveWhite -> MoveWhite
             | canMoveBlack -> MoveBlack
             | otherwise    -> End

-- | Finds all possible moves based on the board state. This function only works
-- for jumps, and is assumed to be called in Phase 2
getPossibleMoves :: Board -> [Move]
getPossibleMoves b = do
   let candidates  = S.toList $ nonempties b
       white_moves = [Jump PWhite x y | x <- candidates, y <- candidates,
                        validMove b (Jump PWhite x y)]
       black_moves = [Jump PBlack x y | x <- candidates, y <- candidates,
                        validMove b (Jump PBlack x y)]
   white_moves ++ black_moves

-- | Attempt to parse a move from a raw input string
parseMove :: String -> TurnState -> Either GameError Move
parseMove s ts = case parse (move ts) s of
  Left _ -> Left MoveParseError
  Right m -> Right m

-- | Given a turnstate, parse and construct a proper move
move :: TurnState -> Parser Move
move ts = choice [jump p, placement c]
  where
    (c,p) = case ts of
      t | t == PlacingWhite || t == MoveWhite -> (White, PWhite)
      t | t == PlacingBlack || t == MoveBlack -> (Black, PBlack)
      PlacingRed -> (Red, PWhite) -- player doesnt matter
      Start -> (Black, PBlack)
      End -> (White, PWhite) --doesn't matter

-- | A placement need to know 1. a coordinate 2. what piece is being placed there
placement :: Piece -> Parser Move
placement p = Place p <$> coord

-- | A jump needs to know the start and end coordinate and who is making the jump
jump :: Player -> Parser Move
jump p = Jump p <$> coord <*> (some space *> string "to" *> some space *> coord)

-- | A coordinate is of the form [A-Z][0-9]+ and is meant to represent x,y
-- coordinates on the dvonn board (which are labeled by the board printer)
coord :: Parser Coordinate
coord = curry Coordinate <$> x <*> y
  where
    x = (\c -> ord c - 64) <$> upper -- X coordinate specified as a capital letter
    y = read <$> some digit -- Y coordinate is a number, i.e. sequence of digits
