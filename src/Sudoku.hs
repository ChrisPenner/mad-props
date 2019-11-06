module Sudoku where

import WFC
import WFC.GraphM
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable


board :: [[Int]]
board = [ x, x, x
        , x, [3], x
        , x, [6, 7], [6]]
  where
    x = [1..9]

-- board' :: [[Int]]
-- board' = [ x, x, x
--          , x, x, x
--          , x, x, x]
--   where
--     x = [1..9]




setup :: GraphM ()
setup = do
    vars <- traverse newPVar board
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
    buildGraph setup
    return ()
