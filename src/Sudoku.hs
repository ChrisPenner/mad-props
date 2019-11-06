{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sudoku where

import WFC
import WFC.GraphM
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Control.Lens


smolBoard :: [[Int]]
smolBoard = [ x, x, x
            , x, [3], x
            , x, [6, 7], [6]]
  where
    x = [1..9]

fullBoard :: [[[Int]]]
fullBoard = [[ x, x, x, x, x, x, x, x, x]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]
            ,[x, x, x, x, x, x, x, x, x ]]
  where
    x = [1..9]

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

setup' :: GraphM ()
setup' = do
    vars <- (traverse . traverse) newPVar fullBoard
    linkBoard vars
    g <- getGraph
    g' <- liftIO $ solve g
    let results = fmap (flip readPVar g') <$> vars
    liftIO $ print results
    return ()

setup :: GraphM ()
setup = do
    vars <- traverse newPVar smolBoard
    traverse_ (\(a, b) -> link a b disjoint) $ do
        (a, b) <- (liftA2 (,) vars vars)
        guard (a /= b)
        return (a, b)
    g <- getGraph
    g' <- liftIO $ solve g
    let results = flip readPVar g' <$> vars
    liftIO $ print results
    return ()

disjoint :: Int -> [Int] -> [Int]
disjoint x = filter (/=x)

run :: IO ()
run = do
    buildGraph setup'
    return ()
