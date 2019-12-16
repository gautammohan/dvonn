-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date: 17 December 2019
-- Assignment: Final Project
------------------------------------------------------------------------------

module IOInterface where

import Defs
import System.IO
import Board
import Data.Set as S hiding (foldr)

-- | Prints top notches of hexagonal board. Takes in a board, number of spaces
-- for offset, and the row number.
printTops :: Board -> Int -> Int -> IO ()
printTops b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
           putStrLn(replicate s ' ' ++ tops)

-- | Prints lower notches of top half of hexagonal board. Takes in a board, 
-- number of spaces for offset, and the row number.
printLowerTops :: Board -> Int -> Int -> IO ()
printLowerTops b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
               fullTop = " \\" ++ tops ++ "/"
           putStrLn(replicate s ' ' ++ fullTop)

-- | Prints bottom notches of hexagonal board. Takes in a board, 
-- number of spaces for offset, and the row number.
printBases :: Board -> Int -> Int -> IO ()
printBases b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               bases = foldr (\x acc-> " \\ /" ++ acc) " " c 
           putStrLn(replicate s ' ' ++ bases)

-- | Prints middle line with stack color and size. Takes in a board, 
-- number of spaces for offset, and the row number.
printMiddlesDvonn :: Board -> Int -> Int -> IO ()
printMiddlesDvonn b s i = do
            let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
                middles = foldr (\x acc -> "|" ++ label x ++ acc) "|" c
            putStrLn(replicate s ' ' ++ middles ++ replicate s ' ' ++ " " ++ show i) where
       label y = case innerstack b y of
          [] -> "   "
          s -> do
                let col = case head s of
                      White -> "W"
                      Black -> "B"
                      Red   -> "R"
                    size = length s
                if size < 10 then col ++ " " ++ show size else col ++ show size

-- | Prints top notches of hexagonal board. Takes in a board, number of spaces
-- for offset, and the row number.
printTopsMini :: Board -> Int -> Int -> IO ()
printTopsMini b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
           putStrLn(replicate s ' ' ++ tops ++ "/")

-- | Prints middle line of mini board with stack color and size. Takes in a board, 
-- number of spaces for offset, and the row number.
printMiddlesMini :: Board -> Int -> Int -> IO ()
printMiddlesMini b s i = do
            let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
                middles = foldr (\x acc -> "|" ++ label x ++ acc) "|" c
            putStrLn(replicate s ' ' ++ middles ++ replicate (5 - s) ' ' ++ show i) where
       label y = case innerstack b y of
          [] -> "   "
          s -> do
                let col = case head s of
                      White -> "W"
                      Black -> "B"
                      Red   -> "R"
                    size = length s
                if size < 10 then col ++ " " ++ show size else col ++ show size


-- | Prints numeric labels for first coordinate on top of board
printTopLabelsDvonn :: IO ()
printTopLabelsDvonn = do
          let letters = ['A' .. 'I'] 
              labels = foldr (\x acc -> "   " ++ [x] ++ acc) "" letters
          putStrLn(replicate 5 ' ' ++ labels)    

-- | Prints bottom labels for first coordinate on top of board
printBottomLabelsDvonn :: IO ()
printBottomLabelsDvonn = do
          let letters = ['C' .. 'K']
              labels = foldr (\x acc -> "   " ++ [x] ++ acc) "" letters
          putStrLn(" " ++ labels)

-- | Prints top labels for first coordinate on top of mini board
printTopLabelsMini :: IO ()
printTopLabelsMini = do
          let letters = ['A' .. 'C']
              labels = foldr (\x acc -> "   " ++ [x] ++ acc) "" letters
          putStrLn(replicate 5 ' ' ++ labels ++ " ")

-- | Prints bottom labels for first coordinate on top of mini board
printBottomLabelsMini :: IO ()
printBottomLabelsMini = putStrLn "A   B   C "

-- | Prints full board
printGridDvonn :: Board -> IO ()
printGridDvonn b = do
            printTopLabelsDvonn
            printTops b 4 1
            printMiddlesDvonn b 4 1
            printTops b 2 2
            printMiddlesDvonn b 2 2
            printTops b 0 3
            printMiddlesDvonn b 0 3
            printLowerTops b 0 4
            printMiddlesDvonn b 2 4
            printLowerTops b 2 5
            printMiddlesDvonn b 4 5
            printBases b 4 5
            printBottomLabelsDvonn

-- | Prints mini board
printGridMini :: Board -> IO ()
printGridMini b = do
            printTopLabelsMini
            printTops b 4 1
            printMiddlesMini b 4 1
            printTopsMini b 2 2
            printMiddlesMini b 2 2
            printTopsMini b 0 3
            printMiddlesMini b 0 3
            printBases b 0 3
            printBottomLabelsMini

-- | Prints full or mini board
printBoard :: Board -> IO ()
printBoard b = if size (coordinates b) == 9 then printGridMini b else printGridDvonn b
