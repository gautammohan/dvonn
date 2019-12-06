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
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ tops)

printLowerTops :: Board -> Int -> Int -> IO ()
printLowerTops b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
               fullTop = " \\" ++ tops ++ "/"
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ fullTop)

printBases :: Board -> Int -> Int -> IO ()
printBases b n s = do
           let ns = [1 .. n]
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               bases = foldr (\x acc-> " \\ /" ++ acc) " " ns
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ bases)

printMiddles :: Board -> Int -> Int -> IO ()
printMiddles b s i = do
            let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
                ss = case s of
                   0 -> []
                   _ -> [1 .. s]
                middles = foldr (\x acc -> "|" ++ label x ++ acc) "|" c
                spaces = foldr (\x acc-> " " ++ acc) "" ss
            putStrLn(spaces ++ middles ++ spaces ++ " " ++ show i) where
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

printGridDvonn :: Board -> IO ()
printGridDvonn b = do
            printTopLabelsDvonn
            printTops b 4 1
            printMiddles b 4 1
            printTops b 2 2
            printMiddles b 2 2
            printTops b 0 3
            printMiddles b 0 3
            printLowerTops b 0 4
            printMiddles b 2 4
            printLowerTops b 2 5
            printMiddles b 4 5
            printBases b 9 4
            printBottomLabelsDvonn

-- | Prints mini board. Can print an empty mini board, for example
-- with the command "printGridMini emptyMini"
--
printTopsMini :: Board -> Int -> Int -> IO ()
printTopsMini b s i = do
           let c = S.toList $ S.filter (\(Coordinate (x,y)) -> y == i) (coordinates b)
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               tops = foldr (\x acc-> " /" ++ r b x ++ "\\" ++ acc) " " c 
                  where r b x = if containsRed b x then "*" else " "
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ tops ++ "/")
           
printGridMini :: Board -> IO ()
printGridMini b = do
            printTops b 4 1
            printMiddles b 4 1
            printTopsMini b 2 2
            printMiddles b 2 2
            printTopsMini b 0 3
            printMiddles b 0 3
            printBases b 3 0

printBoard :: Board -> IO ()
printBoard b = if size (coordinates b) == 9 then printGridMini b else printGridDvonn b
