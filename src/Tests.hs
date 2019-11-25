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
import qualified Data.Map as M hiding (null)
import qualified Data.Set as S
import Debug.Trace (trace)

import Defs
import Board
import Move

-------------------------------------------------------------------------------
-- A main actions to run all the tests
-------------------------------------------------------------------------------
main :: IO ()
main = do
   _ <- runTestTT (TestList [tBoard, tMove])
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

getRandNeighbor :: Board -> Coordinate -> Gen Coordinate
getRandNeighbor b c = oneof $ return <$> S.toList (allNeighbors b c)

-- | Return a random filled board (all pieces are placed)
filledBoard :: Board -> Gen Board
filledBoard b = do
  cs <- shuffle $ S.toList (coordinates b)
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

newtype Dvonn = Dvonn [Board] deriving (Show)
newtype Mini = Mini [Board] deriving (Show)

fill :: [Piece] -> [Coordinate] -> Gen Board
fill ps cs = do
  shuf <- shuffle cs
  return $ Board (M.fromList $ zip cs (Stack . (: []) <$> ps)) (Stack [])

filledDvonn :: Gen Board
filledDvonn = do
  let cs = S.toList (coordinates emptyDvonn)
      ps = replicate 3 Red ++ replicate 23 Black ++ replicate 23 White
  fill ps cs

filledMini :: Gen Board
filledMini = do
  let cs = S.toList (coordinates emptyMini)
      ps = replicate 1 Red ++ replicate 4 Black ++ replicate 4 White
  fill ps cs

genTrace :: Board -> Gen [Board]
genTrace board = do
  let allmoves = getPossibleMoves board
  trace (show $ length allmoves) (return ())
  if null allmoves
    then return [board]
    else do
      move <- oneof $ return <$> allmoves
      let newboard = apply move board
      (board :) <$> genTrace newboard
      -- (board :) <$> frequency [(1, return [newboard]), (3, genTrace newboard)]

instance Arbitrary Dvonn where
  arbitrary = Dvonn <$> (filledDvonn >>= genTrace)

instance Arbitrary Mini where
  arbitrary = Mini <$> (filledMini >>= genTrace)

getMinis :: IO (Board,Move)
getMinis = do
  m <- sample' (arbitrary :: Gen Mini)
  let (Mini bs) = head m
  let b = head bs
  let m = getPossibleMoves b
  return (b,head m)

-------------------------------------------------------------------------------
-- QuickCheck instances for game properties
-------------------------------------------------------------------------------

runMini :: ([Board] -> Bool) -> Mini -> Bool
runMini prop (Mini b) = prop b

-- | Given a sequence [a1,a2,a3], check that a property p holds for all adjacent pairs, i.e. (p a2 a1) && p (a3 a2)
checkBetween :: (a -> a -> Bool) -> [a] -> Bool
checkBetween pred l = getAll $ foldMap (All . uncurry pred) pairs
  where
    pairs = zip l (tail l)
checkTrace = checkBetween

-- Our Arbitrary Coordinate instance should only generate valid coordinates
prop_coordinate_generator :: Coordinate -> Bool
prop_coordinate_generator = validCoordinate emptyDvonn

-- All pieces on the board must have a path to a red piece
--prop_no_disconnected_pieces :: Board -> Bool
--prop_no_disconnected_pieces b = all (`connected` b) (nonempty b)

-- There cannot exist a hole on the board
prop_no_hole :: Board -> Bool
prop_no_hole b = undefined

-- In the move phase, the total number of pieces on the board not including the
-- discard pile must be monotonically decreasing. i.e. given b1 --> b2,
-- numActive b1 > numActive b2
prop_inplay_decrease :: [Board] -> Bool
prop_inplay_decrease = checkTrace ((>) `on` numActivePieces)

-- Over the course of the game, the number of totally surrounded pieces
-- decreases monotonically
prop_surrounded_decrease :: [Board] -> Bool
prop_surrounded_decrease = undefined
-- prop_surrounded_decrease = checkTrace ((>=) `on` numSurrounded)
--   where
--     numSurrounded b = S.size $ S.filter (isSurrounded b) coordinates

-- The number of pieces of each color between the board and discard pile stay
-- consistent after the placing phase. I.e., check that we do not lose any
-- pieces
prop_consistent_sum :: [Board] -> Bool
prop_consistent_sum = checkTrace ((==) `on` totalPieces)
  where
    totalPieces b = numDiscardedPieces b + numActivePieces b

-- The number of empty spaces on the board increases by at least 1 each turn
prop_empty_increase :: [Board] -> Bool
prop_empty_increase l = sort l' == l' && l' == nub l'
  where
    l' = countEmpty <$> l

-------------------------------------------------------------------------------
-- The following tests verify basic functionality in the Board module
-------------------------------------------------------------------------------
smallRedBoard = place emptyDvonn Red (Coordinate (3,3))
surroundedBoard = do
  let b1 = smallRedBoard
      b2 = place b1 White (Coordinate (3,4))
      b3 = place b2 Black (Coordinate (3,2))
      b4 = place b3 Red   (Coordinate (4,3))
      b5 = place b4 White (Coordinate (2,3))
      b6 = place b5 Black (Coordinate (2,2))
  place b6 Red (Coordinate (4,4))

tBoard :: Test
tBoard = TestList [testEmptyBoard, testValidCoordinate, testContainsRed,
                   testAllNeighbors, testNeighborOf, testHasRed, testSurrounded,
                   testNeighbors, testNonempties, testNonempty, testCountEmpty,
                   testNumActivePieces, testCalcWinner]

mini1 = foldr (\e b -> uncurry (place b) e) emptyMini (zip pieces coords)
  where
    coords = S.toList $ coordinates emptyMini
    pieces = [Black,White,Black,White,Red,Black,White,Black,White]
mini2 =
  Board
    (M.fromList
       [ (Coordinate (1, 1), Stack [])
       , (Coordinate (1, 2), Stack [White])
       , (Coordinate (1, 3), Stack [White])
       , (Coordinate (2, 1), Stack [Black, White])
       , (Coordinate (2, 2), Stack [Red])
       , (Coordinate (2, 3), Stack [Black])
       , (Coordinate (3, 1), Stack [White])
       , (Coordinate (3, 2), Stack [Black])
       , (Coordinate (3, 3), Stack [White])
       ])
    (Stack [])

-- TODO : Check that each element is empty, too?
testEmptyBoard :: Test
testEmptyBoard = "empty" ~: TestList [
  length (M.toList $ getMap emptyDvonn) ~?= 49]

testValidCoordinate :: Test
testValidCoordinate = "validDvonnCoordinate" ~: TestList [
  validDvonnCoordinate (Coordinate (-1,-1)) ~?= False,
  validDvonnCoordinate (Coordinate (1,5)) ~?= False,
  validDvonnCoordinate (Coordinate (3,3)) ~?= True]

testContainsRed :: Test
testContainsRed = "containsRed" ~: TestList [
    containsRed smallRedBoard (Coordinate (3,3)) ~?= True,
    containsRed smallRedBoard (Coordinate (3,2)) ~?= False,
    containsRed smallRedBoard (Coordinate (1,5)) ~?= False]

testHasRed :: Test
testHasRed = "hasRed" ~: TestList [
    hasRed smallRedBoard (S.fromList [Coordinate (3,3), Coordinate (1,1)]) ~?= True,
    hasRed emptyDvonn (S.fromList[Coordinate (3,3)]) ~?= False]

testAllNeighbors :: Test
testAllNeighbors = "neighbors" ~: TestList [
    allNeighbors' (Coordinate (-1,-1)) ~?= S.fromList [],
    allNeighbors' (Coordinate (11,3)) ~?=
      S.fromList [Coordinate (10,2), Coordinate (10,3), Coordinate (11,2)],
    allNeighbors' (Coordinate (7,4)) ~?=
      S.fromList [Coordinate (6,3), Coordinate (6,4), Coordinate (7,3),
                Coordinate (7,5), Coordinate (8,4), Coordinate (8,5)]]
  where
    allNeighbors' = allNeighbors emptyDvonn

testNeighborOf :: Test
testNeighborOf = "neighborOf" ~: TestList [
   neighborOf' (Coordinate (5,5)) (Coordinate (5,4)) ~?= True,
   neighborOf' (Coordinate (5,4)) (Coordinate (4,5)) ~?= False,
   neighborOf' (Coordinate (5,5)) (Coordinate (5,6)) ~?= False]
  where
    neighborOf' = neighborOf emptyDvonn

testSurrounded :: Test
testSurrounded = "surrounded" ~: TestList [
  isSurrounded smallRedBoard (Coordinate (3,3)) ~?= False,
  isSurrounded surroundedBoard (Coordinate (3,3)) ~?= True,
  isSurrounded surroundedBoard (Coordinate (2,2)) ~?= False]

testNeighbors :: Test
testNeighbors = "neighbors" ~: TestList [
  neighbors (Coordinate (3,3)) emptyDvonn ~?= S.fromList [],
  neighbors (Coordinate (3,3)) surroundedBoard ~?=
     S.fromList [Coordinate (2,2), Coordinate (2,3), Coordinate (3,2),
                 Coordinate (3,4), Coordinate (4,3), Coordinate (4,4)]]
testConnected :: Test
testConnected = "connected" ~: TestList []

testCalcWinner :: Test
testCalcWinner = "calcWinner" ~: TestList [
   calcWinner smallRedBoard ~?= Nothing,
   calcWinner (place surroundedBoard White (Coordinate (2,1))) ~?= Just PWhite]

testApply :: Test
testApply =
  "apply" ~:
  TestList
    [apply (Move PWhite (Coordinate (1, 1)) (Coordinate (2, 1))) mini1 ~?= mini2]

testGetNextTurn :: Test
testGetNextTurn = "getNextTurn" ~: TestList []

testNonempties :: Test
testNonempties = "testNonEmpty" ~: TestList [
   length (nonempties surroundedBoard) ~?= 7,
   length (nonempties emptyDvonn) ~?= 0]

testNonempty :: Test
testNonempty = "nonempty" ~: TestList [
   nonempty emptyDvonn (Coordinate (1,1)) ~?= False,
   nonempty surroundedBoard (Coordinate (1,1)) ~?= False,
   nonempty surroundedBoard (Coordinate (3,3))~?= True]

testCountEmpty :: Test
testCountEmpty = "testCountEmpty" ~: TestList [
   countEmpty emptyDvonn ~?= 49,
   countEmpty smallRedBoard ~?= 48,
   countEmpty surroundedBoard ~?= 42]

testNumActivePieces :: Test
testNumActivePieces = "numActivePiece" ~: TestList [
   numActivePieces emptyDvonn ~?= 0,
   numActivePieces smallRedBoard ~?= 1,
   numActivePieces surroundedBoard ~?= 7]

-------------------------------------------------------------------------------
-- The following tests verify basic functionality in the Move module
-------------------------------------------------------------------------------

tMove :: Test
tMove = TestList [testPlayerOwns, testIsLinear, testIsOnBoard, testDistance,
                  testGetPossibleMoves]

--Allows for invalid cordinates??
testIsLinear :: Test
testIsLinear = "isLinear" ~: TestList [
   isLinear (Move PWhite (Coordinate (1,1)) (Coordinate (2,2))) ~?= True,
   isLinear (Move PBlack (Coordinate (1,1)) (Coordinate (1,2))) ~?= True,
   isLinear (Move PBlack (Coordinate (2,1)) (Coordinate (1,2))) ~?= False]

testIsOnBoard :: Test
testIsOnBoard = "isOnBoard" ~: TestList [
   isOnBoard' (Move PWhite (Coordinate (1,1)) (Coordinate (2,2))) ~?= True,
   isOnBoard' (Move PWhite (Coordinate (1,5)) (Coordinate (2,2))) ~?= False,
   isOnBoard' (Move PWhite (Coordinate (1,1)) (Coordinate (12,2))) ~?= False]
  where
    isOnBoard' = isOnBoard emptyDvonn

testDistance :: Test
testDistance = "distance" ~: TestList [
   distance (Move PWhite (Coordinate (1,1)) (Coordinate (2,2))) ~?= 1,
   distance (Move PWhite (Coordinate (1,1)) (Coordinate (1,3))) ~?= 2]

testPlayerOwns :: Test
testPlayerOwns = "playerOwns" ~: TestList [
  playerOwns PBlack (Stack [Black, White, Red]) ~?= True,
  playerOwns PWhite (Stack [Red]) ~?= False]

testValidMove :: Test
testValidMove = "validMove" ~: TestList []

testGetPossibleMoves :: Test
testGetPossibleMoves = "getPossibleMoves" ~: TestList [
  getPossibleMoves emptyDvonn ~?= [],
  getPossibleMoves smallRedBoard ~?= [],
  length (getPossibleMoves surroundedBoard) ~?= 12 ]

testParseMove :: Test
testParseMove = "testParseMove" ~: TestList []

-------------------------------------------------------------------------------
-- The following tests verify basic functionality in the Game module
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- The following tests verify the IO functionality
-------------------------------------------------------------------------------
