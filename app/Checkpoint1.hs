module Checkpoint1 where

import Data.Map as M
import Control.Monad.State

newtype Coordinate = Coordinate (Int, Int)

validCoordinate :: Coordinate -> Bool
validCoordinate = undefined

-- start with fixed dimensions of board, possibly generalize to arbitrary dimensions or coordinates
-- need to figure out where discard pile is and initial game pieces pile is
newtype Board = Board {getMap :: M.Map Coordinate Stack}


newtype Stack = Stack [Piece]

data Piece = Red | White | Black


-- initialize every space on the board with an empty stack
initializeGame :: Board
initializeGame = undefined


data TurnState = PlacingRed | PlacingWhite | PlacingBlack | MoveWhite | MoveBlack

-- there is a deterministic turn order for Phase 1 of the game: R R R B W B W ..

-- for the total number of pieces you can figure out the game state by checking
-- which color was placed and whether or not there are any moves of the opposite
-- color or moves of the same color.

data Player = PBlack | PWhite

data Move = Move {player :: Player, start :: Coordinate, end :: Coordinate}

-- check if the move makes geometric sense on the board, and if the pieces allow
-- it to occur
validMove :: Board -> Move -> Bool
validMove = undefined

-- maybe use monad transformers or something to make an ExceptT? and then if
-- moves are invalid we can have custom errors that will display in the REPL and
-- help us figure out what to do next.
type Game = State Board

executeMove :: Move -> Game ()
executeMove = undefined

-- player just helps us figure out which color of valid move to compute first
getPossibleMoves :: Player -> Board -> [Move]
getPossibleMoves = undefined

winner :: Board -> Player
winner = undefined


parseMove :: String -> Move
parseMove = undefined





-- while there are possible moves:
--   player whose turn it is makes a move
--   compute new possible moves
--   if the other color has possible moves, next player is them. otherwise next player stays the same
