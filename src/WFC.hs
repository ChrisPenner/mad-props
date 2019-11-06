module WFC
    (
      solveGraph
    , GraphM
    , PVar
    , newPVar
    , readPVar
    , link

    , disjoint
    , equal
    , require
    ) where

import WFC.Internal.GraphM
import WFC.Internal.Links
