{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Examples.NQueens where

import qualified Data.Set as S
import Props
import Data.Foldable
import Data.List

type Coord = (Int, Int)

killSquares :: Coord -> S.Set Coord -> S.Set Coord
killSquares c = S.filter (killSquare c)

killSquare :: Coord -> Coord -> Bool
killSquare (x, y) (x', y')
  -- Same Row
  | x == x' = False
  -- Same Column
  | y == y' = False
  -- Same Diagonal 1
  | x - x' == y - y' = False
  -- Same Diagonal 2
  | x + y == x' + y' = False
  | otherwise = True

setup :: Int -> GraphM s [PVar s (S.Set Coord)]
setup n = do
    -- All possible grid locations
    let locations = S.fromList [(x, y) | x <- [0..n - 1], y <- [0..n - 1]]
    let queens = replicate n locations
    queenVars <- traverse newPVar queens
    let queenPairs = [(a, b) | a <- queenVars, b <- queenVars, a /= b]
    for_ queenPairs $ \(a, b) -> link a b killSquares
    return queenVars

solve :: Int -> IO ()
solve n = do
    (vars, g) <- solveGraph (setup n)
    let results = readPVar g <$> vars
    putStrLn $ printSolution n results


-- solveAll :: Int -> IO ()
-- solveAll n = do
--     (vars, gs :: [Graph s]) <- solveGraphAll (setup n)
--     let results = zipWith (<$>) (readPVar <$> gs) (repeat vars)
--     traverse_ (putStrLn . printSolution n) results


chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr go
  where
    go [] = Nothing
    go xs = Just (take n xs, drop n xs)

printSolution :: Int -> [Coord] -> String
printSolution n (S.fromList -> qs) =
    let str = toChar . (`S.member` qs) <$> [(x, y) | x <- [0..n-1], y <- [0..n-1]]
     in unlines . chunksOf n $ str
  where
    toChar True = 'Q'
    toChar False = '.'

