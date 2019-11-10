{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.STSudoku where

import ST.Internal.GraphM
import Data.Foldable
import Text.RawString.QQ (r)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.List

txtToBoard :: [String] -> [[S.Set Int]]
txtToBoard = (fmap . fmap) possibilities
  where
    possibilities :: Char -> S.Set Int
    possibilities '.' = S.fromList [1..9]
    possibilities a = S.fromList [read [a]]

boardToText :: [[Int]] -> String
boardToText xs = unlines . fmap concat $ (fmap . fmap) show xs

easyBoard :: [T.Text]
easyBoard = T.lines $ T.dropWhile (=='\n') [r|
..3.42.9.
.9..6.5..
5......1.
..17..285
..8...1..
329..87..
.3......1
..5.9..2.
.8.21.6..|]

hardestBoard :: [String]
hardestBoard = tail . lines $ [r|
8........
..36.....
.7..9.2..
.5...7...
....457..
...1...3.
..1....68
..85...1.
.9....4..|]


expected :: T.Text
expected = [r|613542897
897361542
542987316
461739285
758426139
329158764
236874951
175693428
984215673
|]

hardestExpected :: T.Text
hardestExpected = [r|812753649
943682175
675491283
154237896
369845721
287169534
521974368
438526917
796318452
|]


rowsOf :: [[a]] -> [[a]]
rowsOf = id
colsOf :: [[a]] -> [[a]]
colsOf = transpose
blocksOf :: [[a]] -> [[a]]
blocksOf = chunksOf 9 . concat . concat . fmap transpose . chunksOf 3 . transpose

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr go
  where
    go [] = Nothing
    go xs = Just (take n xs, drop n xs)


linkBoard :: [[PVar s (S.Set Int)]] -> GraphM s ()
linkBoard xs = do
    let rows = rowsOf xs
    let cols = colsOf xs
    let blocks = blocksOf xs
    for_ (rows <> cols <> blocks) $ \region -> do
        let uniquePairings = [(a, b) | a <- region, b <- region, a /= b]
        for_ uniquePairings $ \(a, b) -> link a b disj
  where
    disj :: Ord a => a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs

setup :: [[S.Set Int]]-> GraphM s [[PVar s (S.Set Int)]]
setup board = do
    vars <- (traverse . traverse) newPVar board
    linkBoard vars
    return vars

solvePuzzle :: [String] -> IO ()
solvePuzzle puz = do
    vars <- solveGraph (setup $ txtToBoard puz)
    results <- (traverse . traverse) readPVar vars
    putStrLn $ boardToText results

hardST :: IO ()
hardST = do solvePuzzle hardestBoard

-- test :: IO ()
-- test = do
--     result <- solvePuzzle hardestBoard
--     if result == expected then putStrLn "Success!"
--                           else putStrLn "UH OH!"
--     return ()
