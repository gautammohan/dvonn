-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date:
-- Assigment: Final Project
-------------------------------------------------------------------------------

module Tests where

import Test.HUnit hiding (Path)
import Test.QuickCheck
import Data.Function
import Data.List
import Data.Monoid
import Control.Monad
import Data.Map as M hiding (null)
import qualified Data.Set as S

import Defs
import Board
import Move

-------------------------------------------------------------------------------
-- A main actions to run all the tests
-------------------------------------------------------------------------------
main :: IO ()
main = do
   _ <- runTestTT (TestList [tBlock, tMove])
   quickCheck prop_coordinate_generator
   return ()

--quickCheck $ prop_no_disconnected_pieces emptyBoard

-- TODO: Generalize beyond emptyBoard once we have full arbitrary intances
--  quickCheck $ prop_empty_increase
--       (Trace [])
-------------------------------------------------------------------------------
-- A framework for randomized testing
-------------------------------------------------------------------------------

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

getRandNeighbor :: Coordinate -> Gen Coordinate
getRandNeighbor c = oneof $ return <$> S.toList (allNeighbors c)


-- | Return a random filled board (all pieces are placed)
filledBoard :: Gen Board
filledBoard = do
  cs <- shuffle $ S.toList coordinates
  let ps =
        Stack . (: []) <$>
        replicate 3 Red ++ replicate 23 Black ++ replicate 23 White
  let m = M.fromList $ zip cs ps
  return $ Board m (Stack [])

newtype Path = Path { getPath :: [Coordinate]}

-- | A path is a sequence of adjacent coordinates without a cycle
instance Arbitrary Path where
  arbitrary = undefined

-- TODO figure out how to use this to test the neighbor property
-- aka get it working

newtype Trace = Trace { getTrace :: [Board] } deriving (Show)

instance Arbitrary Trace where
  arbitrary = Trace <$> (filledBoard >>= helper)
    where
      helper :: Board -> Gen [Board]
      helper board = do
        let allmoves = getPossibleMoves board
        if null allmoves
          then return [board]
          else do
            move <- oneof $ return <$> allmoves
            let newboard = apply move board
            (board :) <$>
              frequency [(1, return [newboard]), (3, helper newboard)]

-------------------------------------------------------------------------------
-- QuickCheck instances for game properties
-------------------------------------------------------------------------------

checkBetween :: (a -> a -> Bool) -> [a] -> Bool
checkBetween pred l = getAll $ foldMap (All . uncurry pred) pairs
  where
    pairs = zip l (tail l)

checkTrace pred = checkBetween pred . getTrace

-- Our Arbitrary Coordinate instance should only generate valid coordinates
prop_coordinate_generator :: Coordinate -> Bool
prop_coordinate_generator = validCoordinate

-- All pieces on the board must have a path to a red piece
--prop_no_disconnected_pieces :: Board -> Bool
--prop_no_disconnected_pieces b = all (`connected` b) (nonempty b)

-- There cannot exist a hole on the board
prop_no_hole :: Board -> Bool
prop_no_hole b = undefined

-- In the move phase, the total number of pieces on the board not including
-- the discard pile must be monotonically decreasing
prop_inplay_decrease :: Trace -> Bool
prop_inplay_decrease = checkTrace pred
  where
    pred = (<) `on` numActivePieces

-- Over the course of the game, the number of totally surrounded pieces
-- decreases monotonically
prop_surrounded_decrease :: Trace -> Bool
prop_surrounded_decrease = undefined

-- The number of pieces of each color between the board and discard pile stay
-- consistent after the placing phase. I.e., check that we do not lose any
-- pieces
prop_consistent_sum :: Board -> Bool
prop_consistent_sum = undefined

-- The number of empty spaces on the board increases by at least 1 each turn
prop_empty_increase :: Trace -> Bool
prop_empty_increase (Trace l) = sort l' == l' && l' == nub l'
  where
    l' = countEmpty <$> l

-------------------------------------------------------------------------------
-- The following tests verify basic functionality in the Board module
-------------------------------------------------------------------------------

tBlock :: Test
tBlock = TestList [testEmptyBoard, testValidCoordinate]

-- TODO : Check that each element is empty, too?
testEmptyBoard :: Test
testEmptyBoard = "empty" ~: TestList [
  length (M.toList $ getMap emptyBoard) ~?= 50]

testValidCoordinate :: Test
testValidCoordinate = "validCoordinate" ~: TestList [
  validCoordinate (Coordinate (-1,-1)) ~?= False,
  validCoordinate (Coordinate (1,5)) ~?= False,
  validCoordinate (Coordinate (3,3)) ~?= True]

--testAreNeighbors :: Test
--testAreNeighbors = "areNeighbors" ~: TestList [
--  areNeighbors (Coordinate (11,1)) (Coordinate (11,2)) ~?= True,
--  areNeighbors (Coordinate (3,4)) (Coordinate (4,3)) ~?= False,
--  areNeighbors (Coordinate (-1,-1)) (Coordinate (1,1)) ~?= False,
--  areNeighbors (Coordinate (11,3)) (Coordinate (11,4)) ~?= False]

testContainsRed :: Test
testContainsRed = "containsRed" ~: TestList []

testNeighbors :: Test
testNeighbors = "neighbors" ~: TestList []

testSurrounded :: Test
testSurrounded = "surrounded" ~: TestList []

testConnected :: Test
testConnected = "connected" ~: TestList []

testCalcWinner :: Test
testCalcWinner = "calcWinner" ~: TestList []

testApply :: Test
testApply = "apply" ~: TestList []

testGetNextTurn :: Test
testGetNextTurn = "getNextTurn" ~: TestList []

testNonEmpty :: Test
testNonEmpty = "testNonEmpty" ~: TestList []

testCountEmpty :: Test
testCountEmpty = "testCountEmpty" ~: TestList []

-------------------------------------------------------------------------------
-- The following tests verify basic functionality in the Move module
-------------------------------------------------------------------------------

tMove :: Test
tMove = TestList [testPlayerOwns]

testIsLinear :: Test
testIsLinear = "isLinear" ~: TestList []

testIsOnBoard :: Test
testIsOnBoard = "isOnBoard" ~: TestList []

testDistance :: Test
testDistance = "distance" ~: TestList []

testPlayerOwns :: Test
testPlayerOwns = "playerOwns" ~: TestList [
  playerOwns PBlack (Stack [Black, White, Red]) ~?= True,
  playerOwns PWhite (Stack [Red]) ~?= False]

testValidMove :: Test
testValidMove = "validMove" ~: TestList []

testPlacePiece :: Test
testPlacePiece = "placePiece" ~: TestList []

testGetPossibleMoves :: Test
testGetPossibleMoves = "getPossibleMoves" ~: TestList []

testParseMove :: Test
testParseMove = "testParseMove" ~: TestList []

-------------------------------------------------------------------------------
-- The following tests verify basic functionality in the Game module
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- The following tests verify the IO functionality
-------------------------------------------------------------------------------
