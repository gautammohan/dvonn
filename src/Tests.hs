-------------------------------------------------------------------------------
-- Authors: Emily Diana and Gautam Mohan
-- Date: 17 December 2019
-- Assigment: Final Project
-------------------------------------------------------------------------------

module Tests where

import Test.HUnit hiding (Path)
import Test.QuickCheck
import Data.Function
import Data.List
import Data.Monoid
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import IOInterface

import Defs
import Board
import Move

-------------------------------------------------------------------------------
-- A main actions to run all the tests
-------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Running Unit Tests"
  runTestTT (TestList [tBoard, tMove])
  putStrLn "Checking individual board properties for mini traces"
  mapM_ quickCheck $ forAll miniTrace . onTrace <$> boardProps
  putStrLn "Checking each individual board prop for dvonn pairs"
  mapM_ quickCheck $ forAll dvonnPairs . onPair <$> boardProps
  putStrLn "Checking the composition of all board properties for mini traces"
  quickCheck $ forAll miniTrace (onTrace $ mconcat boardProps)
  putStrLn
    "Checking the composition of all board properties for dvonn traces (warning...this takes awhile)"
  quickCheck $ forAll dvonnTrace (onTrace $ mconcat boardProps)
  return ()

-------------------------------------------------------------------------------
-- A framework for randomized testing
-------------------------------------------------------------------------------

-- | Helper functions for checking valid coordinates for different boards
validDvonnCoordinate :: Coordinate -> Bool
validDvonnCoordinate = validCoordinate emptyDvonn
validMiniCoordinate :: Coordinate -> Bool
validMiniCoordinate = validCoordinate emptyMini

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
          | x == 1  = rand `mod` 3
          | x == 2  = rand `mod` 4
          | x == 10 = (rand `mod` 4) + 1
          | x == 11 = (rand `mod` 3) + 2
          | otherwise = rand `mod` 5

miniPieces = replicate 1 Red ++ replicate 4 Black ++ replicate 4 White
dvonnPieces = replicate 3 Red ++ replicate 23 Black ++ replicate 23 White

-- split a list of pieces into stacks, more small stacks than large stacks
randStacks :: (Int,Int) -> (Int,Int) -> [Piece] -> Gen [Stack]
randStacks _ _ [] = return []
randStacks small large l = do
  ps <- shuffle l
  bound <- frequency [(3, return small), (1, return large)]
  i <- choose bound
  let (stack, rest) = splitAt i ps
  (Stack stack :) <$> randStacks small large rest

-- | Given a list of stacks, place them randomly on the given coordinates,
-- making sure at least one valid move exists after cleaning up the board
--
-- NOTE: if no possible placement of the stack list can result in a board with
-- valid moves, this will diverge. Possible diverging inputs could be: a single
-- stack, many stacks that are too high to move. This function will perform very
-- slowly if the average stack height is very high or the number of stacks is
-- low, since it will decrease the space of boards with valid moves
-- significantly
placeRandomly :: [Stack] -> [Coordinate] -> Gen Board
placeRandomly stacks coords = do
  cs <- shuffle coords
  let map = M.fromList $ zip cs (stacks ++ repeat (Stack []))
  let b = cleanup $ Board map (Stack [])
  if null $ getPossibleMoves b
    then placeRandomly stacks coords
    else return b

-- | A mini dvonn board that generates smaller traces and is easier to debug
newtype Mini = Mini {getMini :: Board} deriving (Show)
-- | The full dvonn board
newtype Dvonn = Dvonn {getDvonn :: Board} deriving (Show)

-- | generate Mini boards with at least one possible move
instance Arbitrary Mini where
  arbitrary = do
    cs <- shuffle . S.toList $ coordinates emptyMini
    stacks <- randStacks (1, 1) (2, 3) miniPieces
    Mini <$> placeRandomly stacks (S.toList $ coordinates emptyMini)

-- | generate Mini boards with at least one possible move
instance Arbitrary Dvonn where
  arbitrary = do
    cs <- shuffle . S.toList $ coordinates emptyDvonn
    stacks <- randStacks (1, 3) (4, 8) dvonnPieces
    Dvonn <$> placeRandomly stacks (S.toList $ coordinates emptyDvonn)

-- | Generates a random board and one random valid move on that board
pairOf :: Gen Board -> Gen (Board, Move)
pairOf g = do
  let (Single no_holes) = prop_no_hole
  b <- g `suchThat` no_holes
  m <- oneof $ return <$> getPossibleMoves b
  return (b,m)

dvonnBoard = getDvonn <$> arbitrary
miniBoard = getMini <$> arbitrary
miniPairs = pairOf miniBoard
dvonnPairs = pairOf dvonnBoard

-- | A trace is a sequence of board generated by randomly making valid moves
-- from each board to the next. It is primarily used to check "global" game
-- properties that should hold as the game is played
genTrace :: Board -> Gen [Board]
genTrace board = do
  let allmoves = getPossibleMoves board
  trace (show $ length allmoves) (return ())
  if null allmoves
    then return [board]
    else do
      move <- oneof $ return <$> allmoves
      let newboard = apply move board
      (board :) <$> frequency [(1, return [newboard]), (3, genTrace newboard)]

-- | Generate a random full board, aka a piece placed at every position
filledDvonn = placeRandomly stacks coords
  where
    coords = S.toList $ coordinates emptyDvonn
    stacks = Stack . (:[]) <$> dvonnPieces

filledMini = placeRandomly stacks coords
  where
    coords = S.toList $ coordinates emptyMini
    stacks = Stack . (:[]) <$> miniPieces

-- | Though traces can be generated from any board, traces are longer and more
-- realistic when they are generated from random board placements
miniTrace = filledMini >>= genTrace
dvonnTrace = filledDvonn >>= genTrace

-----------------------------------
-- Prop Combinators
-----------------------------------
type Trace = [Board]
type Pair = (Board, Move)

-- | A combinator to check a property on a board and the board produced by
-- making a move. Good for random "unit testing" of board state correctness.
liftPair :: (Board -> Board -> Bool) -> (Board,Move) -> Bool
liftPair pred (b,m) = pred b (apply m b)

-- | Given a sequence [a1,a2,a3], check that a property p holds for all adjacent
-- pairs, i.e. (p a2 a1) && p (a3 a2).
liftTrace :: (a -> a -> Bool) -> [a] -> Bool
liftTrace pred l = getAll $ foldMap (All . uncurry pred) pairs
  where
    pairs = zip l (tail l)

data BoardProp = Preserves (Board -> Board -> Bool) | Single (Board -> Bool)

onTrace :: BoardProp -> Trace -> Bool
onTrace (Preserves f) t = liftTrace f t
onTrace (Single f) t = all f t

onPair :: BoardProp -> Pair -> Bool
onPair (Preserves f) p = liftPair f p
onPair (Single f) (b, m) = f b && f (apply m b)

instance Semigroup BoardProp where
  Preserves f <> Preserves g = Preserves $ \b1 b2 -> f b1 b2 && g b1 b2
  Preserves f <> Single g = Preserves $ \b1 b2 -> f b1 b2 && g b1 && g b2
  Single f <> Preserves g = Preserves g <> Single f
  Single f <> Single g = Single $ \b -> f b && g b

instance Monoid BoardProp where
  mempty = Single $ const True

-------------------------------------------------------------------------------
-- QuickCheck instances for game properties
-------------------------------------------------------------------------------
-- | Our Arbitrary Coordinate instance should only generate valid coordinates
prop_coordinate_generator :: Coordinate -> Bool
prop_coordinate_generator = validCoordinate emptyDvonn

-- | The random boards generated should be standard sizes (mini vs full-size)
prop_gen_mini_sane = forAll miniBoard $ (9 ==) . S.size . coordinates
prop_gen_dvonn_sane = forAll dvonnBoard $ (49 ==) . S.size . coordinates


boardProps =
  [ prop_valid_move
  , prop_nonempty_decreasing
  , prop_no_hole
  , prop_inplay_decreasing
  , prop_surrounded_decrease
  , prop_consistent_sum
  , prop_empty_increase
  , prop_no_disconnects
  , prop_num_components
  , prop_unique_components
  ]
-- | all posible moves must be valid from a random board
prop_valid_move = Single $ \b -> getAll $ foldMap (All . validMove b) (getPossibleMoves b)

-- | the number of nonempty spaces must decrease after making a move
prop_nonempty_decreasing = Preserves ((>) `on` (S.size . nonempties))

-- | There cannot exist a hole on the board. This means that all nonempty spaces
-- on the board cannot have 6 neighbors (where a neighbor is an active stack)
prop_no_hole =
  Single $ \b ->
    getAll $ foldMap (All . (< 6) . S.size . flip neighbors b) (empties b)

-- In the move phase, the total number of pieces on the board not including the
-- discard pile must be monotonically decreasing. i.e. given b1 --> b2,
-- numActive b1 > numActive b2
prop_inplay_decreasing = Preserves ((>=) `on` numActivePieces)

-- Over the course of the game, the number of totally surrounded pieces
-- decreases monotonically
prop_surrounded_decrease = Preserves ((>=) `on` numSurrounded)
  where
    numSurrounded b = S.size $ S.filter (isSurrounded b) (coordinates b)

-- The number of pieces of each color between the board and discard pile stay
-- consistent after the placing phase. I.e., check that we do not lose any
-- pieces
prop_consistent_sum = Preserves ((==) `on` totalPieces)
  where
    totalPieces b = numDiscardedPieces b + numActivePieces b

-- The number of empty spaces on the board increases by at least 1 each turn
prop_empty_increase = Preserves ((<) `on` countEmpty)

prop_no_disconnects =
  Single $ \b -> getAll $ foldMap (All . hasRed b) (allComponents b)

-- | The number of components can never exceed 3, the maximum number of red
-- spaces allowed
prop_num_components = Single $ (< 4) . length . allComponents

-- | All components from the board must have no overlapping coordinates
prop_unique_components =
  Single $ \b ->
    let cs = allComponents b
        cpairs = [(c1, c2) | c1 <- cs, c2 <- cs, c1 /= c2]
        no_overlap = S.null . uncurry S.intersection
     in all no_overlap cpairs

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

mini1 =
  Board
    (M.fromList
       [ (Coordinate (1, 1), Stack [White])
       , (Coordinate (1, 2), Stack [Black])
       , (Coordinate (1, 3), Stack [White])
       , (Coordinate (2, 1), Stack [Black])
       , (Coordinate (2, 2), Stack [Red])
       , (Coordinate (2, 3), Stack [Black])
       , (Coordinate (3, 1), Stack [White])
       , (Coordinate (3, 2), Stack [Black])
       , (Coordinate (3, 3), Stack [White])
       ])
    (Stack [])
mini2 =
  Board
    (M.fromList
       [ (Coordinate (1, 1), Stack [])
       , (Coordinate (1, 2), Stack [Black])
       , (Coordinate (1, 3), Stack [White])
       , (Coordinate (2, 1), Stack [White, Black])
       , (Coordinate (2, 2), Stack [Red])
       , (Coordinate (2, 3), Stack [Black])
       , (Coordinate (3, 1), Stack [White])
       , (Coordinate (3, 2), Stack [Black])
       , (Coordinate (3, 3), Stack [White])
       ])
    (Stack [])

move1 = Jump PWhite (Coordinate (1,1)) (Coordinate (2,1))

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
      S.fromList [Coordinate (10,2), Coordinate (10,3), Coordinate (11,4)],
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

testCalcWinner :: Test
testCalcWinner = "calcWinner" ~: TestList [
   calcWinner smallRedBoard ~?= Nothing,
   calcWinner (place surroundedBoard White (Coordinate (2,1))) ~?= Just PWhite]

testApply :: Test
testApply =
  "apply" ~:
  TestList
    [apply move1 mini1 ~?= mini2]

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
                  testValidMove, testGetPossibleMoves]

testIsLinear :: Test
testIsLinear = "isLinear" ~: TestList [
   isLinear (Jump PWhite (Coordinate (1,1)) (Coordinate (2,2))) ~?= True,
   isLinear (Jump PBlack (Coordinate (1,1)) (Coordinate (1,2))) ~?= True,
   isLinear (Jump PBlack (Coordinate (2,1)) (Coordinate (1,2))) ~?= False]

testIsOnBoard :: Test
testIsOnBoard = "isOnBoard" ~: TestList [
   isOnBoard' (Jump PWhite (Coordinate (1,1)) (Coordinate (2,2))) ~?= True,
   isOnBoard' (Jump PWhite (Coordinate (1,5)) (Coordinate (2,2))) ~?= False,
   isOnBoard' (Jump PWhite (Coordinate (1,1)) (Coordinate (12,2))) ~?= False]
  where
    isOnBoard' = isOnBoard emptyDvonn

testDistance :: Test
testDistance = "distance" ~: TestList [
   distance (Jump PWhite (Coordinate (1,1)) (Coordinate (2,2))) ~?= 1,
   distance (Jump PWhite (Coordinate (1,1)) (Coordinate (1,3))) ~?= 2]

testPlayerOwns :: Test
testPlayerOwns = "playerOwns" ~: TestList [
  playerOwns PBlack (Stack [Black, White, Red]) ~?= True,
  playerOwns PWhite (Stack [Red]) ~?= False]

testValidMove :: Test
testValidMove = "validMove" ~: TestList [
  validMove mini1 m ~?= True,
  validMove mini2 m ~?= False]
 where
  m = Jump PWhite  (Coordinate (1,1)) (Coordinate (2,2))

testGetPossibleMoves :: Test
testGetPossibleMoves = "getPossibleMoves" ~: TestList [
  getPossibleMoves emptyDvonn ~?= [],
  getPossibleMoves smallRedBoard ~?= [],
  length (getPossibleMoves surroundedBoard) ~?= 12 ]
