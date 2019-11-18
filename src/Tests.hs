module Tests where

import Test.HUnit
import Test.QuickCheck
import Data.List
import Control.Monad


import Defs
import Board

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
-- instance Arbitrary Tests.Path where
--   arbitrary = undefined
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

-- the number of empty spaces on the board increases by at least 1 eavh turn
prop_empty_incease :: Trace -> Bool
prop_empty_incease (Trace l) = sort l' == l' && l' == nub l'
  where
    l' = countEmpty <$> l

newtype Trace = Trace [Board]

getPossibleMoves = undefined

instance Arbitrary Trace where
  arbitrary = undefined
--   arbitrary = do
--     b <- arbitrary :: Gen Board
--     helper b where
--     undefined
--   where
--     helper :: Gen Board -> [Gen Board]
--     helper board' = do
--        board <- board'
--        let allmoves = getPossibleMoves board
--        if null allmoves then return []
--        let move = oneof $ return <$> allmoves
--        let b' = apply move board
--        oneof $ frequency [(1, return []), (5, b': helper)]
