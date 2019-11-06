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
import WFC.Types

type Vertex = Int
type DFilter = Dynamic
type DSuperPos = Dynamic
type DChoice = Dynamic

data Quantum =
    forall f a. (Foldable f, Typeable a, Typeable f) => Quantum
        { options   :: SuperPos f a
        }

instance Show Quantum where
  show _ = "Quantum"

forceDyn :: forall a. Typeable a => Dynamic -> a
forceDyn d =
    fromMaybe (error ("Expected type: " <> expected <> " but Dyn was type: " <> show d)) (fromDynamic d)
  where
    expected = show (typeOf (undefined :: a))

data Graph =
    Graph { _vertices :: !(IM.IntMap (Quantum, IM.IntMap DFilter))
          , _vertexCount :: !Int
          } deriving Show


makeLenses ''Graph

emptyGraph :: Graph
emptyGraph = Graph mempty 0

-- newGraph :: forall k e a. (Eq k, Hashable k, Hashable e, Eq e) => [(k, a)] -> [(k, k, e)] -> Graph
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

valueAt :: Vertex -> Traversal' Graph Quantum
valueAt n = vertices . ix n . _1
{-# INLINE valueAt #-}

hmAsList :: (Eq k, Hashable k) => Iso' (HM.HashMap k v ) [(k, v)]
hmAsList = iso HM.toList HM.fromList
{-# INLINABLE hmAsList #-}

imAsList :: Iso' (IM.IntMap v ) [(Vertex, v)]
imAsList = iso IM.toList IM.fromList
{-# INLINABLE imAsList #-}

edges :: Vertex -> Traversal' Graph (IM.IntMap DFilter)
edges n = vertices . ix n . _2
{-# INLINABLE edges #-}

edgeBetween :: Vertex -> Vertex -> Traversal' Graph DFilter
edgeBetween from' to' = edges from' . ix to'
{-# INLINABLE edgeBetween #-}

values :: IndexedTraversal' Vertex Graph Quantum
values = vertices . itraversed <. _1
{-# INLINABLE values #-}

edgesFrom :: Vertex -> Traversal' Graph (Vertex, DFilter)
edgesFrom n = edges n . imAsList . traversed
{-# INLINE edgesFrom #-}

setChoiceQ :: (Typeable f, Typeable a, Foldable f) => SuperPos f a -> Quantum -> Quantum
setChoiceQ s (Quantum _) = Quantum s

entropyOfQ :: Quantum -> Maybe Int
entropyOfQ (Quantum (Unknown xs)) = Just $ length xs
entropyOfQ _ = Nothing
