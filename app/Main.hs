module Main where

import Game (evalGame, dvonn, startState, phase2)
import Control.Monad.State (evalState)
import Defs

main :: IO ()
main = do
  w <-  evalGame startState dvonn
  case w of
    Right (winner, _) -> putStrLn $ "The winner is: " ++ show winner
    Left _ -> error "something went catastrophically wrong"

main' :: IO ()
main' = do
  w <-  evalGame filledState phase2
  case w of
    Right (winner, _) -> putStrLn $ "The winner is: " ++ show winner
    Left _ -> error "something went catastrophically wrong"

filledState = GameState filledBoard PlacingBlack Phase2
filledBoard =
  read
    "Board {getMap = fromList [(Coordinate (1,1),Stack {getStack = [Black]}),(Coordinate (1,2),Stack {getStack = [Red]}),(Coordinate (1,3),Stack {getStack = [Black]}),(Coordinate (2,1),Stack {getStack = [Black]}),(Coordinate (2,2),Stack {getStack = [Black]}),(Coordinate (2,3),Stack {getStack = [White]}),(Coordinate (2,4),Stack {getStack = [Black]}),(Coordinate (3,1),Stack {getStack = [White]}),(Coordinate (3,2),Stack {getStack = [White]}),(Coordinate (3,3),Stack {getStack = [Black]}),(Coordinate (3,4),Stack {getStack = [Black]}),(Coordinate (3,5),Stack {getStack = [Black]}),(Coordinate (4,1),Stack {getStack = [White]}),(Coordinate (4,2),Stack {getStack = [Black]}),(Coordinate (4,3),Stack {getStack = [White]}),(Coordinate (4,4),Stack {getStack = [White]}),(Coordinate (4,5),Stack {getStack = [White]}),(Coordinate (5,1),Stack {getStack = [White]}),(Coordinate (5,2),Stack {getStack = [White]}),(Coordinate (5,3),Stack {getStack = [Black]}),(Coordinate (5,4),Stack {getStack = [White]}),(Coordinate (5,5),Stack {getStack = [White]}),(Coordinate (6,1),Stack {getStack = [White]}),(Coordinate (6,2),Stack {getStack = [Black]}),(Coordinate (6,3),Stack {getStack = [Black]}),(Coordinate (6,4),Stack {getStack = [White]}),(Coordinate (6,5),Stack {getStack = [Black]}),(Coordinate (7,1),Stack {getStack = [Black]}),(Coordinate (7,2),Stack {getStack = [White]}),(Coordinate (7,3),Stack {getStack = [White]}),(Coordinate (7,4),Stack {getStack = [White]}),(Coordinate (7,5),Stack {getStack = [Black]}),(Coordinate (8,1),Stack {getStack = [White]}),(Coordinate (8,2),Stack {getStack = [Black]}),(Coordinate (8,3),Stack {getStack = [Black]}),(Coordinate (8,4),Stack {getStack = [White]}),(Coordinate (8,5),Stack {getStack = [Black]}),(Coordinate (9,1),Stack {getStack = [White]}),(Coordinate (9,2),Stack {getStack = [Black]}),(Coordinate (9,3),Stack {getStack = [Black]}),(Coordinate (9,4),Stack {getStack = [Red]}),(Coordinate (9,5),Stack {getStack = [White]}),(Coordinate (10,2),Stack {getStack = [White]}),(Coordinate (10,3),Stack {getStack = [Black]}),(Coordinate (10,4),Stack {getStack = [White]}),(Coordinate (10,5),Stack {getStack = [White]}),(Coordinate (11,3),Stack {getStack = [Red]}),(Coordinate (11,4),Stack {getStack = [Black]}),(Coordinate (11,5),Stack {getStack = [Black]})], getDiscard = Stack {getStack = []}}" :: Board
