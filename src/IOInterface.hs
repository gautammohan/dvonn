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
import Data.Set as S hiding (foldr)

printTops :: Int -> Int -> IO ()
printTops n s = do
           let ns = [1 .. n]
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               tops = foldr (\x acc-> " / \\" ++ acc) " " ns
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ tops)

printBases :: Int -> Int -> IO ()
printBases n s = do
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
            putStrLn(spaces ++ middles) where
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

printGridDvonn :: Board -> IO ()
printGridDvonn b = do
            printTops 9 4
            printMiddles b 4 1
            printTops 10 2
            printMiddles b 2 2
            printTops 11 0
            printMiddles b 0 3
            printBases 11 0
            printMiddles b 2 4
            printBases 10 2
            printMiddles b 4 5
            printBases 9 4

-- | Prints mini board. Can print an empty mini board, for example
-- with the command "printGridMini emptyMini"
--
printTopsMini :: Int -> Int -> IO ()
printTopsMini n s = do
           let ns = [1 .. n]
               ss = case s of
                  0 -> []
                  _ -> [1 ..s]
               tops = foldr (\x acc-> " / \\" ++ acc) " " ns
               spaces = foldr (\x acc-> " " ++ acc) "" ss
           putStrLn(spaces ++ tops ++ "/")

printGridMini :: Board -> IO ()
printGridMini b = do
            printTops 3 4
            printMiddles b 4 1
            printTopsMini 3 2
            printMiddles b 2 2
            printTopsMini 3 0
            printMiddles b 0 3
            printBases 3 0

printBoard :: Board -> IO ()
printBoard b = if size (coordinates b) == 9 then printGridMini b else printGridDvonn b


--Placeholder IO parsing stuff
playGame :: Board -> IO ()
playGame b = go b where
  go board = do
    putBoard board
    putStr "imp > "
    str <- getLine
    case str of   --use parsing here
      ('m':' ':'(':x1:',':y1:')':' ':'(':x2:',':y2:')':[]) -> return () 
      _   -> putStrLn "?" >> go board
  putBoard :: Board -> IO ()
  putBoard g = printBoard g

