module Move where

import Board (Board, Coordinate (Coordinate))
import Defs
  ( GameError(ParseError)
  , GameState
  , Move
  , Move(Move)
  , Player(PBlack, PWhite)
  )

parseMove :: String -> Either GameError Move
parseMove "next" = Right $ Move PWhite c c
  where
    c = Coordinate (0,0)
parseMove _ = Left ParseError

validMove :: GameState -> Move -> Bool
validMove = undefined
