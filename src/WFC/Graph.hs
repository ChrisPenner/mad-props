{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module WFC.Graph where
    -- ( Graph
    -- , newGraph
    -- , valueAt
    -- , hmAsList
    -- , imAsList
    -- , edgesFrom
    -- , vertexAt
    -- , valueAtKey
    -- , edges
    -- , vertices
    -- , labels
    -- , Vertex
    -- ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Lens
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Data.List

type Vertex = Int
type DFilter = Dynamic
type DSuperPos = Dynamic
type DChoice = Dynamic

forceDyn :: forall a. Typeable a => Dynamic -> a
forceDyn d =
    fromMaybe (error ("Expected type: " <> expected <> " but Dyn was type: " <> show d)) (fromDynamic d)
  where
    expected = show (typeOf (undefined :: a))

data Graph k =
    Graph { _vertices :: !(IM.IntMap (DSuperPos, IM.IntMap DFilter))
          , _labels :: !(HM.HashMap k Int)
          }

makeLenses ''Graph

-- instance FunctorWithIndex Int (Graph k) where
-- instance FoldableWithIndex Int (Graph k) where
-- instance TraversableWithIndex Int (Graph k) where
--     itraversed = vertices . itraversed

-- newGraph :: forall k e a. (Eq k, Hashable k, Hashable e, Eq e) => [(k, a)] -> [(k, k, e)] -> Graph k
-- newGraph vertexKeys edgeList = Graph vs es ls
--   where
--     vs :: IM.IntMap a
--     vs = IM.fromList $ fmap (first (fromJust . (HM.lookup ?? ls))) vertexKeys
--     es :: IM.IntMap (HM.HashMap e Vertex)
--     es = IM.unionsWith HM.union $ fmap toEdge edgeList
--     toEdge :: (k, k, e) -> IM.IntMap (HM.HashMap e Vertex)
--     toEdge (HM.lookup ?? ls -> Just from', HM.lookup ?? ls -> Just to', e) = IM.singleton from' (HM.singleton e to')
--     toEdge _ = mempty
--     ls :: HM.HashMap k Int
--     ls = HM.fromList (zip (fst <$> vertexKeys) [0..])

valueAt :: Vertex -> Traversal' (Graph k) DSuperPos
valueAt n = vertices . ix n . _1
{-# INLINE valueAt #-}

hmAsList :: (Eq k, Hashable k) => Iso' (HM.HashMap k v ) [(k, v)]
hmAsList = iso HM.toList HM.fromList
{-# INLINABLE hmAsList #-}

imAsList :: Iso' (IM.IntMap v ) [(Vertex, v)]
imAsList = iso IM.toList IM.fromList
{-# INLINABLE imAsList #-}

edges :: Vertex -> Traversal' (Graph k) (IM.IntMap DFilter)
edges n = vertices . ix n . _2
{-# INLINABLE edges #-}

edgeBetween :: Vertex -> Vertex -> Traversal' (Graph k) DFilter
edgeBetween from' to' = edges from' . ix to'
{-# INLINABLE edgeBetween #-}

values :: IndexedTraversal' Vertex (Graph k) DSuperPos
values = vertices . itraversed <. _1
{-# INLINABLE values #-}

edgesFrom :: Vertex -> Traversal' (Graph k) (Vertex, DFilter)
edgesFrom n = edges n . imAsList . traversed
{-# INLINE edgesFrom #-}

vertexAt :: (Eq k, Hashable k) => k -> Fold (Graph k) Vertex
vertexAt k = labels . folding (HM.lookup k)
{-# INLINE vertexAt #-}

valueAtKey :: (Eq k, Hashable k) => k -> Traversal' (Graph k) DSuperPos
valueAtKey k f graph' =
    case vertex of
        Nothing -> pure graph'
        Just v -> graph' & valueAt v %%~ f
    where
      vertex = graph' ^? vertexAt k
{-# INLINE valueAtKey #-}

propagate :: forall k. (Vertex, DChoice) -> Graph k -> Graph k
propagate (from', choice) g = foldl' step g allEdges
    where
      allEdges :: [(Vertex, DFilter)]
      allEdges = g ^.. edgesFrom from'
      step :: Graph k -> (Vertex, DFilter) -> Graph k
      step g' e = propagateSingle choice e g'
{-# INLINABLE propagate #-}

propagateSingle :: DChoice -> (Vertex, DFilter) -> Graph k -> Graph k
propagateSingle v (to', dfilter) g = g & valueAt to' %~ dynApp (dynApp dfilter v)
{-# INLINABLE propagateSingle #-}
