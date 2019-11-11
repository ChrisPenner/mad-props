{-|
Module      : Props
Description : Monadic DSL for building constraint solvers using basic propagators.
Copyright   : (c) Chris Penner, 2019
License     : BSD3

This module exports everything you should need to get started. Take a look at 'Examples.NQueens' or 'Examples.Sudoku' to see how to get started.
-}
module Props
    (
    -- * Initializing problems
      Prop
    , PropT
    , PVar
    , newPVar

    -- * Finding Solutions
    , solve
    , solveAll

    -- * Constraining variables
    , constrain
    , disjoint
    , equal
    , require
    ) where

import Props.Internal.PropT
import Props.Internal.Links
