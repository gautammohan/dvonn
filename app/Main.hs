module Main where

import Game (evalGame, dvonn, startState)
import Control.Monad.State (evalState)

main :: IO ()
main = do
  w <-  evalGame startState dvonn
  case w of
    Right (winner, _) -> putStrLn $ "The winner is: " ++ show winner
    Left _ -> error "something went catastrophically wrong"
