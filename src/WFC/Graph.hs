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
{-# LANGUAGE TypeApplications #-}

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
import Control.Lens
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import WFC.Types
import Data.Coerce

type DFilter = Dynamic
type DChoice = Dynamic
type Vertex' = Int
newtype Vertex = Vertex Int
  deriving (Show, Eq, Ord)

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

data Graph s =
    Graph { _vertices :: !(IM.IntMap (Quantum, IM.IntMap DFilter))
          , _vertexCount :: !Int
          } deriving Show


makeLenses ''Graph

emptyGraph :: Graph s
emptyGraph = Graph mempty 0

valueAt :: Vertex -> Lens' (Graph s) Quantum
valueAt (Vertex n) = singular (vertices . ix n . _1)
{-# INLINE valueAt #-}

imAsList :: Iso' (IM.IntMap v ) [(Vertex', v)]
imAsList = iso IM.toList IM.fromList
{-# INLINABLE imAsList #-}

edges :: Vertex -> Lens' (Graph s) (IM.IntMap DFilter)
edges (Vertex n) = singular (vertices . ix n . _2)
{-# INLINABLE edges #-}

edgeBetween :: Vertex -> Vertex -> Lens' (Graph s) (Maybe DFilter)
edgeBetween from' (Vertex to') = edges from' . at to'
{-# INLINABLE edgeBetween #-}

values :: IndexedTraversal' Vertex (Graph s) Quantum
values = vertices . reindexed (coerce @Int @Vertex) itraversed <. _1
{-# INLINABLE values #-}

edgesFrom :: Vertex -> Traversal' (Graph s) (Vertex, DFilter)
edgesFrom n = edges n . imAsList . traversed . coerced
{-# INLINE edgesFrom #-}

setChoiceQ :: (Typeable f, Typeable a, Foldable f) => SuperPos f a -> Quantum -> Quantum
setChoiceQ s (Quantum _) = Quantum s

entropyOfQ :: Quantum -> Maybe Int
entropyOfQ (Quantum (Unknown xs)) = Just $ length xs
entropyOfQ _ = Nothing
