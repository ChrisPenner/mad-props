module Props
    (
      solveGraph
    -- , solveGraphAll
    , GraphM
    , Graph
    , PVar
    , newPVar
    , readPVar
    , link

    , disjoint
    , equal
    , require
    ) where

import Props.Internal.GraphM
import Props.Internal.Graph
import Props.Internal.Links
