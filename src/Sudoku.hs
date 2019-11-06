{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Sudoku where

import WFC
import WFC.GraphM
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Control.Lens
import Text.RawString.QQ (r)


txtToBoard :: [String] -> [[[Int]]]
txtToBoard = (fmap . fmap) inflate
  where
    inflate :: Char -> [Int]
    inflate '.' = [1..9]
    inflate a = [read [a]]

boardToText :: [[Int]] -> String
boardToText xs = unlines . fmap concat $ (fmap . fmap) show xs

easyBoard :: [String]
easyBoard = lines $ dropWhile (=='\n') [r|
..3.42.9.
.9..6.5..
5......1.
..17..285
..8...1..
329..87..
.3......1
..5.9..2.
.8.21.6..|]

expected :: String
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

linkBoard :: [[PVar [] Int]] -> GraphM ()
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

setup :: GraphM ()
setup = do
    vars <- (traverse . traverse) newPVar (txtToBoard easyBoard)
    linkBoard vars
    g <- getGraph
    g' <- liftIO $ solve g
    let results = fmap (flip readPVar g') <$> vars
    let sResults = boardToText results
    liftIO $ putStrLn sResults
    liftIO $ if sResults == expected
       then putStrLn "SUCCESS"
       else putStrLn "UNEXPECTED!"
    return ()

disjoint :: Int -> [Int] -> [Int]
disjoint x = filter (/=x)

run :: IO ()
run = do
    buildGraph setup
    return ()
