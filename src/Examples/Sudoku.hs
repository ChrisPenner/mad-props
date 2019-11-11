{-|
Module      : Examples.Sudoku
Description : A simple sudoku solver
Copyright   : (c) Chris Penner, 2019
License     : BSD3

Click 'Source' on a function to see how it's implemented!
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.Sudoku where

import Props
import Data.Foldable
import Text.RawString.QQ (r)
import qualified Data.Set as S
import Data.List
import Data.Functor.Compose

-- | Convert a textual board into a board containing sets of cells of possible numbers
txtToBoard :: [String] -> [[S.Set Int]]
txtToBoard = (fmap . fmap) possibilities
  where
    possibilities :: Char -> S.Set Int
    possibilities '.' = S.fromList [1..9]
    possibilities a = S.fromList [read [a]]

-- | Convert a board to a string.
boardToText :: [[Int]] -> String
boardToText xs = unlines . fmap concat $ (fmap . fmap) show xs

-- | An easy to solve sudoku board
easyBoard :: [[S.Set Int]]
easyBoard = txtToBoard . tail . lines $ [r|
..3.42.9.
.9..6.5..
5......1.
..17..285
..8...1..
329..87..
.3......1
..5.9..2.
.8.21.6..|]

hardestBoard :: [[S.Set Int]]
hardestBoard = txtToBoard . tail . lines $ [r|
8........
..36.....
.7..9.2..
.5...7...
....457..
...1...3.
..1....68
..85...1.
.9....4..|]


-- | Get a list of all rows in a board
rowsOf :: [[a]] -> [[a]]
rowsOf = id

-- | Get a list of all columns in a board
colsOf :: [[a]] -> [[a]]
colsOf = transpose

-- | Get a list of all square blocks in a board
blocksOf :: [[a]] -> [[a]]
blocksOf = chunksOf 9 . concat . concat . fmap transpose . chunksOf 3 . transpose
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf n = unfoldr go
      where
        go [] = Nothing
        go xs = Just (take n xs, drop n xs)


-- | Given a board of 'PVar's, link the appropriate cells with 'disjoint' constraints
linkBoardCells :: [[PVar (S.Set Int)]] -> Prop ()
linkBoardCells xs = do
    let rows = rowsOf xs
    let cols = colsOf xs
    let blocks = blocksOf xs
    for_ (rows <> cols <> blocks) $ \region -> do
        let uniquePairings = [(a, b) | a <- region, b <- region, a /= b]
        for_ uniquePairings $ \(a, b) -> constrain a b disj
  where
    disj :: Ord a => a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs

-- | Given a sudoku board, apply the necessary constraints and return a result board of
-- 'PVar's. We wrap the result in 'Compose' because 'solve' requires a Functor over 'PVar's
constrainBoard :: [[S.Set Int]]-> Prop (Compose [] [] (PVar (S.Set Int)))
constrainBoard board = do
    vars <- (traverse . traverse) newPVar board
    linkBoardCells vars
    return (Compose vars)

-- Solve a given sudoku board and print it to screen
solvePuzzle :: [[S.Set Int]] -> IO ()
solvePuzzle puz = do
    -- We know it will succeed, but in general you should handle failure safely
    let Just (Compose results) = solve $ constrainBoard puz
    putStrLn $ boardToText results

solveEasyPuzzle :: IO ()
solveEasyPuzzle = solvePuzzle easyBoard
