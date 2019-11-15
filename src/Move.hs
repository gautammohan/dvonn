module Move where

import Board (Board, Coordinate (Coordinate))
import Defs (Player (PWhite, PBlack), Move, GameState, GameError, Move(Move))


parseMove :: String -> Either GameError Move
parseMove _ = Right $ Move PWhite c c
  where
    c = Coordinate (0,0)

validMove :: GameState -> Move -> Bool
validMove = undefined
