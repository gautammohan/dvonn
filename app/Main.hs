module Main where

import Game
import Control.Monad.State (evalState)
import Defs
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  w <-  evalGame startState dvonn
  case w of
    Right (winner, _) -> putStrLn $ "The winner is: " ++ show winner
    Left _ -> error "something went catastrophically wrong"

