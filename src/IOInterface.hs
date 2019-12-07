-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assignment: Final Project
------------------------------------------------------------------------------

module IOInterface where

import Defs
import System.IO
import Board
import Data.Set as S hiding (foldr)

printTops :: Board -> Int -> Int -> IO ()
printTops b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
           putStrLn(replicate s ' ' ++ tops)

printLowerTops :: Board -> Int -> Int -> IO ()
printLowerTops b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
               fullTop = " \\" ++ tops ++ "/"
           putStrLn(replicate s ' ' ++ fullTop)

printBases :: Board -> Int -> Int -> IO ()
printBases b n s = do
           let ns = [1 .. n]
               bases = foldr (\x acc-> " \\ /" ++ acc) " " ns
           putStrLn(replicate s ' ' ++ bases)

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

-- | Prints normal Dvonn sized board. Can print an emty board, for example
-- with the command "printGridDvonn emptyDvonn"
printTopLabelsDvonn :: IO ()
printTopLabelsDvonn = putStrLn("        A   B   C   D   E   F   G   H   I")

printBottomLabelsDvonn :: IO ()
printBottomLabelsDvonn = putStrLn("    C   D   E   F   G   H   I   J   K")

printTopLabelsMini :: IO ()
printTopLabelsMini = putStrLn("        A   B   C ")

printBottomLabelsMini :: IO ()
printBottomLabelsMini = putStrLn("A   B   C ")

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
            printBases b 9 4
            printBottomLabelsDvonn

-- | Prints mini board. Can print an empty mini board, for example
-- with the command "printGridMini emptyMini"
--
printTopsMini :: Board -> Int -> Int -> IO ()
printTopsMini b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
           putStrLn(replicate s ' ' ++ tops ++ "/")
           
printGridMini :: Board -> IO ()
printGridMini b = do
            printTopLabelsMini
            printTops b 4 1
            printMiddlesMini b 4 1
            printTopsMini b 2 2
            printMiddlesMini b 2 2
            printTopsMini b 0 3
            printMiddlesMini b 0 3
            printBases b 3 0
            printBottomLabelsMini

printBoard :: Board -> IO ()
printBoard b = if size (coordinates b) == 9 then printGridMini b else printGridDvonn b
