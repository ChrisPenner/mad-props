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
{-# LANGUAGE GADTs #-}

module Props.Internal.Graph
    ( Graph
    , valueAt
    , imAsList
    , edgesFrom
    , edges
    , vertices
    , Vertex(..)
    , Quantum(..)
    , SuperPos(..)
    , _Observed
    , _Unknown
    , DFilter
    , DChoice
    , forceDyn
    , values
    , entropyOfQ
    , emptyGraph
    , edgeBetween
    , vertexCount
    , superPos
    ) where

import qualified Data.IntMap.Strict as IM
import Control.Lens
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Data.Typeable.Lens

type DFilter = Dynamic
type DChoice = Dynamic
type Vertex' = Int
newtype Vertex = Vertex Int
  deriving (Show, Eq, Ord)

data SuperPos f a where
    Observed :: Foldable f => a -> SuperPos f a
    Unknown :: Foldable f => f a -> SuperPos f a

instance Show (SuperPos f a) where
  show (Observed _) = "Observed"
  show (Unknown _) = "Unknown"

_Unknown :: Foldable f => Prism' (SuperPos f a) (f a)
_Unknown = prism' embed match
  where
    embed = Unknown
    match (Unknown f) = Just f
    match _ = Nothing

_Observed :: Foldable f => Prism' (SuperPos f a) a
_Observed = prism' embed match
  where
    embed = Observed
    match (Observed a) = Just a
    match _ = Nothing

data Quantum =
    forall f a. (Show (SuperPos f a), Typeable f, Typeable a, Foldable f) => Quantum
        { options   :: SuperPos f a
        }

superPos :: (Typeable f, Typeable a) => Traversal' Quantum (SuperPos f a)
superPos f (Quantum o) = Quantum <$> (o & _cast %%~ f)

instance Show Quantum where
  show (Quantum xs) = "Quantum " <> show xs

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

valueAt :: Vertex -> Lens' (Graph) Quantum
valueAt (Vertex n) = singular (vertices . ix n . _1)
{-# INLINE valueAt #-}

imAsList :: Iso' (IM.IntMap v ) [(Vertex', v)]
imAsList = iso IM.toList IM.fromList
{-# INLINABLE imAsList #-}

edges :: Vertex -> Lens' (Graph) (IM.IntMap DFilter)
edges (Vertex n) = singular (vertices . ix n . _2)
{-# INLINABLE edges #-}

edgeBetween :: Vertex -> Vertex -> Lens' (Graph) (Maybe DFilter)
edgeBetween from' (Vertex to') = edges from' . at to'
{-# INLINABLE edgeBetween #-}

values :: Traversal' (Graph) (Vertex, Quantum)
values = vertices . imAsList . traversed . alongside coerced _1
{-# INLINABLE values #-}

edgesFrom :: Vertex -> Traversal' (Graph) (Vertex, DFilter)
edgesFrom n = edges n . imAsList . traversed . coerced
{-# INLINE edgesFrom #-}

entropyOfQ :: Quantum -> (Maybe Int)
entropyOfQ (Quantum (Unknown xs)) = Just $ length xs
entropyOfQ _ = Nothing
