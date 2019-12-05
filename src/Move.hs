-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
-------------------------------------------------------------------------------

module Move where

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
validMove b m@Jump{} = case M.lookup (start m) (getMap b) of
   Just (Stack s)  -> isOnBoard b m  && isLinear m  &&
              not (isSurrounded b (start m)) &&
              distance m == length s && playerOwns (player m) (Stack s)
   _       -> False
validMove b m@(Place _ coord) = coord `S.member` empties b

--We only need move or Turnstate
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

-- | Places a piece on an empty coordinate in the first phase of the game
--
--TODO: I'm not sure how to fill this in with the Game Monad. This is something
--we will probably want to replace later.
--Figure out how to check if this placement is valid,
--probably in the game monad.
--placePiece :: Board -> Piece -> Coordinate -> Board
--placePiece b p c = Board $ M.insert c (Stack [p]) (getMap b)

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

parseMove :: String -> TurnState -> Either GameError Move
parseMove s ts = case parse (move ts) s of
  Left _ -> Left MoveParseError
  Right m -> Right m

move :: TurnState -> Parser Move
move ts = choice [jump p, placement c]
  where
    (c,p) = case ts of
      t | t == PlacingWhite || t == MoveWhite -> (White, PWhite)
      t | t == PlacingBlack || t == MoveBlack -> (Black, PBlack)
      PlacingRed -> (Red, PWhite) -- player doesnt matter
      Start -> (Black, PBlack)
      End -> (White, PWhite) --doesn't matter

placement :: Piece -> Parser Move
placement p = Place p <$> coord

jump :: Player -> Parser Move
jump p = Jump p <$> coord <*> (some space *> string "to" *> some space *> coord)

coord :: Parser Coordinate
coord = curry Coordinate <$> x <*> y
  where
    x = (\c -> ord c - 64) <$> upper -- X coordinate specified as a capital letter
    y = read <$> some digit -- Y coordinate is a number, i.e. sequence of digits
