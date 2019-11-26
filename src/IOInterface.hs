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

printTops :: Int -> Int -> IO ()
printTops n s = do
           let ns = [1 .. n]
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               tops = foldr (\x acc-> " / \\" ++ acc) " " ns
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ tops ++ spaces)

printBases :: Int -> Int -> IO ()
printBases n s = do
           let ns = [1 .. n]
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               bases = foldr (\x acc-> " \\ /" ++ acc) " " ns
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ bases ++ spaces)
               
--tops = " / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\ "
--parametrize this with coordinates next
printMiddles :: Int -> Int -> IO ()
printMiddles n s = do
            let ns = [1 .. n]
                ss = case s of
                   0 -> []
                   _ -> [1 .. s]
                middles = foldr (\x acc -> "|   " ++ acc) "|" ns
                spaces = foldr (\x acc-> " " ++ acc) "" ss
            putStrLn(spaces ++ middles ++ spaces)

--  putStrLn("|   |   |   |   |   |   |   |   |   |")

printGridDvonn :: Board -> IO ()
printGridDvonn b = do
            printTops 9 2
            printMiddles 9 2
            printBases 9 2
