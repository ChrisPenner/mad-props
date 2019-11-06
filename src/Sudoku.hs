{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Sudoku where

import WFC
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Control.Lens
import Text.RawString.QQ (r)
import qualified Data.Text as T
import qualified Data.IntSet as IS

txtToBoard :: [T.Text] -> [[IS.IntSet]]
txtToBoard = (fmap . fmap) inflate . fmap T.unpack
  where
    inflate :: Char -> IS.IntSet
    inflate '.' = IS.fromList [1..9]
    inflate a = IS.fromList [read [a]]

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
hardestBoard = lines $ dropWhile (=='\n') [r|
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

puzzles :: [[T.Text]]
puzzles = fmap toPuzzle . tail . T.lines $ [r|
..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9
.......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6...
.2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9..
........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........
12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8
1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1
.......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7.....
12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8
..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5.
1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6.
..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7..
....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7
4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........
7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5...........
3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6.....
........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3
.......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6...
.......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6..
1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1
.....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67.....|]
  where
    toPuzzle :: T.Text -> [T.Text]
    toPuzzle = T.chunksOf 9

linkBoard :: [[PVar s (IS.IntSet)]] -> GraphM s ()
linkBoard xs = do
    let coordPairs = do r <- [0..8]
                        c <- [0..8]
                        return (r, c)
    for_ (liftA2 (,) coordPairs coordPairs) $ \(a, b) ->
        when (sameRow a b || sameCol a b || sameBlock a b)
            $ linkPair a b
  where
    linkPair (r, c) (r', c') = link (xs ^?! ix r . ix c) (xs ^?! ix r' . ix c') disjoint
    sameRow (r, _) (r', _)  = r == r'
    sameCol (_, c) (_, c')  = c == c'
    sameBlock (r, c) (r', c') = (r `div` 3 == r' `div` 3) && (c `div` 3 == c' `div` 3)

setup :: [[IS.IntSet]]-> GraphM s [[PVar s (IS.IntSet)]]
setup board = do
    vars <- (traverse . traverse) newPVar board
    linkBoard vars
    return vars

solvePuzzle :: [T.Text] -> IO T.Text
solvePuzzle puz = do
    (vars, g) <- solveGraph (setup $ txtToBoard puz)
    let results = (fmap . fmap) (readPVar g) vars
    let sResults = boardToText results
    liftIO $ putStrLn sResults
    return (T.pack sResults)

solveAll :: IO ()
solveAll = do
    traverse_ solvePuzzle (take 5 puzzles)

disjoint :: Int -> IS.IntSet -> IS.IntSet
disjoint x xs = IS.delete x xs

test :: IO ()
test = do
    result <- solvePuzzle easyBoard
    if result == expected then putStrLn "Success!"
                          else putStrLn "UH OH!"
    return ()
