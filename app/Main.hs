module Main where

import Examples.Sudoku as S
import Examples.NQueens as NQ

main :: IO ()
main = do
    S.solveEasyPuzzle
-- main = NQ.solve 12
-- main = hardLogic
