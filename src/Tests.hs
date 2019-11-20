-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date: 
-- Assigment: Final Project
-------------------------------------------------------------------------------

module Tests where

import Test.HUnit
import Test.QuickCheck
import Data.List
import Control.Monad
import Data.Map as M hiding (null)

import Defs
import Board
import Move

validMoves :: Board -> [Move]
validMoves = undefined

instance Arbitrary Piece where
  arbitrary = oneof $ return <$> [Red, White, Black]

instance Arbitrary Coordinate where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    let boundedX = 1 + x `mod` 11
    let boundedY = 1 + getValidY boundedX y
    return $ Coordinate (boundedX, boundedY)
      where
        getValidY x rand
          | x == 1 || x == 11 = rand `mod` 3
          | x == 2 || x == 10 = rand `mod` 4
          | otherwise = rand `mod` 5

-- our Arbitrary Coordinate instance should only generate valid coordinates
prop_coordinate_generator :: Coordinate -> Bool
prop_coordinate_generator = validCoordinate

getRandNeighbor :: Coordinate -> Gen Coordinate
getRandNeighbor c = do
  d <- oneof $ return . Coordinate <$>
    [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1)]
  return $ c + d

newtype Path = Path [Coordinate]

-- TODO figure out how to use this to test the neighbor property aka get it working
instance Arbitrary Tests.Path where
   arbitrary = undefined
--     where
--       check :: Gen Coordinate -> Maybe (Gen Coordinate, Gen Coordinate)
--       check c = do
--         coord <- c
--         if validCoordinate coord then
--           Just (c, getRandNeighbor c)
--           else Nothing
--       -- check c = if validCoordinate c then return (return c,getRandNeighbor c) else Nothing

-- all pieces on the board must have a path to a red piece
prop_no_disconnected_pieces :: Board -> Bool
prop_no_disconnected_pieces b = all (`connected` b) (nonempty b)

-- the number of empty spaces on the board increases by at least 1 each turn
prop_empty_incease :: Trace -> Bool
prop_empty_incease (Trace l) = sort l' == l' && l' == nub l'
  where
    l' = countEmpty <$> l

newtype Trace = Trace [Board]


getPossibleMoves :: Board -> [Move]
getPossibleMoves = undefined

instance Arbitrary Trace where
   arbitrary = undefined
--   arbitrary = do
--     b <- arbitrary :: Gen Board
--     helper b


helper :: Board -> Gen [Board]
helper board = do
  let allmoves = getPossibleMoves board
  if null allmoves
    then return [board]
    else do
    move <- oneof $ return <$> allmoves
    let newboard = apply move board
    frequency [ (1,return [board,newboard])
              , (3, do
                    nextboards <- helper newboard
                    return $ board : nextboards)]

--Check that each element is empty, too? 
testEmptyBoard :: Test
testEmptyBoard = "empty" ~: TestList [
  length (M.toList $ getMap emptyBoard) ~?= 50]

testValidCoordinate :: Test
testValidCoordinate = "validCoordinate" ~: TestList [
  validCoordinate (Coordinate (-1,-1)) ~?= False,
  validCoordinate (Coordinate (1,5)) ~?= False,
  validCoordinate (Coordinate (3,3)) ~?= True]

testAreNeighbors :: Test
testAreNeighbors = "areNeighbors" ~: TestList [
  areNeighbors (Coordinate (11,1)) (Coordinate (11,2)) ~?= True,
  areNeighbors (Coordinate (3,4)) (Coordinate (4,3)) ~?= False,
  areNeighbors (Coordinate (-1,-1)) (Coordinate (1,1)) ~?= False,
  areNeighbors (Coordinate (11,3)) (Coordinate (11,4)) ~?= False]

--Fill in more once better board generation tools
testContainsRed :: Test
testContainsRed = "containsRed" ~: TestList []

--Fill in more once have better board generation tools
testNeighbors :: Test
testNeighbors = "neighbors" ~: TestList []

testConnected :: Test
testConnected = "connected" ~: TestList []

testPlayerOwns :: Test
testPlayerOwns = "playerOwns" ~: TestList [
  playerOwns PBlack (Stack [Black, White, Red]) ~?= True,
  playerOwns PWhite (Stack [Red]) ~?= False]

testValidMove :: Test
testValidMove = "validMove" ~: TestList []

