{-|
Module      : Examples.NQueens
Description : An implementation of the classic N-Queens constraint puzzle.
Copyright   : (c) Chris Penner, 2019
License     : BSD3

Click 'Source' on a function to see how it's implemented!
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Examples.NQueens where

import qualified Data.Set as S
import Props
import Data.Foldable
import Data.List

-- | A board coordinate
type Coord = (Int, Int)

-- | Given a number of queens, constrain them to not overlap
constrainQueens :: Int -> Prop [PVar (S.Set Coord)]
constrainQueens n = do
    -- All possible grid locations
    let locations = S.fromList [(x, y) | x <- [0..n - 1], y <- [0..n - 1]]
    -- Each queen could initially be placed anywhere
    let queens = replicate n locations
    -- Make a PVar for each queen's location
    queenVars <- traverse newPVar queens
    -- Each pair of queens must not overlap
    let queenPairs = [(a, b) | a <- queenVars, b <- queenVars, a /= b]
    for_ queenPairs $ \(a, b) -> require (\x y -> not $ overlapping x y) a b
    return queenVars

-- | Check whether two queens overlap with each other (i.e. could kill each other)
overlapping :: Coord -> Coord -> Bool
overlapping (x, y) (x', y')
  -- Same Row
  | x == x' = True
  -- Same Column
  | y == y' = True
  -- Same Diagonal 1
  | x - x' == y - y' = True
  -- Same Diagonal 2
  | x + y == x' + y' = True
  | otherwise = False

-- | Print an nQueens puzzle to a string.
showSolution :: Int -> [Coord] -> String
showSolution n (S.fromList -> qs) =
    let str = toChar . (`S.member` qs) <$> [(x, y) | x <- [0..n-1], y <- [0..n-1]]
     in unlines . chunksOf n $ str
  where
    toChar :: Bool -> Char
    toChar True = 'Q'
    toChar False = '.'

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf n = unfoldr go
      where
        go [] = Nothing
        go xs = Just (take n xs, drop n xs)

-- | Solve and print an N-Queens puzzle
nQueens :: Int -> IO ()
nQueens n = do
    let Just results = solve (constrainQueens n)
    putStrLn $ showSolution n results

-- | Solve and print all possible solutions of an N-Queens puzzle
-- This will include duplicates.
nQueensAll :: Int -> IO ()
nQueensAll n = do
    let results = solveAll (constrainQueens n)
    traverse_ (putStrLn . showSolution n) results

