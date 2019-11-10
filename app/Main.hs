module Main where

import Examples.STSudoku
import Examples.Sudoku
import Examples.NQueens as NQ
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["st"] -> hardST
        ["logic"] -> hardLogic
-- main = NQ.solve 12
