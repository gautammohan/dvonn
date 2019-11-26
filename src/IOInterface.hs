-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
------------------------------------------------------------------------------

module IOInterface where

import Defs
import System.IO
import Board
import Tests
--TODO: What is the type signature here?
--parseMove :: Move -> ??
parseMove = undefined

--TODO: What is the type signature here?
--printBoard :: Board -> ??
printHexagon :: Board -> Coordinate -> IO ()
printHexagon b c = do
               let s = innerstack b c
                   color = case s of
                     (x:xs) -> case head s of
                        White -> "W"
                        Black -> "B"
                        _     -> "R"
                     _      -> " "
                   len = case s of 
                     (x:xs) -> show (length s)
                     _      -> " "
               putStrLn(" --- ")
               putStrLn("/ " ++ color ++ " \\")
               putStrLn("\\ " ++ len ++ " /")
               putStrLn(" --- ")

printHexagon2 :: Board -> Coordinate -> IO ()
printHexagon2 b c = do
               let s = innerstack b c
                   color = case s of
                     (x:xs) -> case head s of
                        White -> "W"
                        Black -> "B"
                        _     -> "R"
                     _      -> " "
                   len = case s of 
                     (x:xs) -> show (length s)
                     _      -> " "
               putStrLn(" /'\\ ")
               putStrLn("|" ++ color ++ len ++ " |")
               putStrLn(" \\-/ ")

printTop :: IO ()
printTop = putStrLn(" / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\ ")

printSecondLine :: Board -> IO ()
printSecondLine b = putStrLn("|   |   |   |   |   |   |   |   |   |")

printGrid :: Board -> IO ()
printGrid b = do
            printTop
            printSecondLine b
