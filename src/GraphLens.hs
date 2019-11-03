{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module GraphLens where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Control.Lens hiding ((&), Context)
import Control.Applicative

allContexts :: Fold (Gr a e) (Context a e)
allContexts = folding (ufold (:) [])

ctxAt :: Node -> Traversal' (Gr a e) (Context a e)
ctxAt n f gr =
    case match n gr of
        (Just ctx, gr') -> liftA2 (&) (f ctx) (pure gr')
        (Nothing, gr') -> pure gr'

ctxLabel :: Lens' (Context a e) a
ctxLabel = _3

ctxSuc :: Lens' (Context a e) (Adj e)
ctxSuc = _4

ctxPre :: Lens' (Context a e) (Adj e)
ctxPre = _1

adjNode :: Lens' (b, Node) Node
adjNode = _2
