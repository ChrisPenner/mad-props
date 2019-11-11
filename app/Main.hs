module Main where

import Examples.Sudoku
import Examples.NQueens as NQ

main :: IO ()
main = NQ.nQueens 8
-- main = NQ.solve 12
-- main = hardLogic
