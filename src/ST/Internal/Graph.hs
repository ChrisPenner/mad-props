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

module ST.Internal.Graph where
    -- ( Graph
    -- , valueAt
    -- , imAsList
    -- , edgesFrom
    -- , edges
    -- , vertices
    -- , Vertex(..)
    -- , Quantum(..)
    -- , SuperPos(..)
    -- , _Observed
    -- , _Unknown
    -- , DFilter
    -- , DChoice
    -- , forceDyn
    -- , values
    -- , entropyOfQ
    -- , emptyGraph
    -- , edgeBetween
    -- , vertexCount
    -- , superPos
    -- ) where

import qualified Data.IntMap.Strict as IM
import Control.Lens
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Data.Typeable.Lens
import Data.MonoTraversable
import Data.IORef
import Data.Function (on)
import Control.Applicative

type DFilter = Dynamic
type DChoice = Dynamic

data SuperPos f where
    Observed :: MonoFoldable f => Element f -> SuperPos f
    Unknown :: MonoFoldable f => f -> SuperPos f

instance Show (SuperPos f) where
  show (Observed _) = "Observed"
  show (Unknown _) = "Unknown"

data Quantum =
    forall f. (Show (SuperPos f), Typeable f, Typeable (Element f), MonoFoldable f) => Quantum
        { options   :: SuperPos f
        }

instance Show Quantum where
  show (Quantum xs) = "Quantum " <> show xs


type VertexID = Int
data Vertex = Vertex { _vertexID :: Int
                     , _vertexQuantum ::  IORef Quantum
                     , _vertexEdges :: IORef [(Vertex, DFilter)]
                     }

instance Show Vertex where
  show (Vertex id' _ _) = show "(Vertex " <> show id' <> ")"

instance Eq Vertex where
  (==) = (==) `on` _vertexID

instance Ord Vertex where
  (<=) = (<=) `on` _vertexID

makeLenses ''Vertex

data Graph s =
    Graph { _verticesMap :: !(IM.IntMap Vertex)
          , _vertexCount :: !Int
          } deriving Show

makeLenses ''Graph


_Unknown :: (Show f, Show (Element f), MonoFoldable f) => Prism' (SuperPos f) f
_Unknown = prism' embed match
  where
    embed = Unknown
    match (Unknown f) = Just f
    match _ = Nothing

_Observed :: (Show (Element f), MonoFoldable f) => Prism' (SuperPos f) (Element f)
_Observed = prism' embed match
  where
    embed = Observed
    match (Observed a) = Just a
    match _ = Nothing

superPos :: (Typeable f, Typeable (Element f), MonoFoldable f) => Traversal' Quantum (SuperPos f)
superPos f (Quantum o) = Quantum <$> (o & _cast %%~ f)

forceDyn :: forall a. Typeable a => Dynamic -> a
forceDyn d =
    fromMaybe (error ("Expected type: " <> expected <> " but Dyn was type: " <> show d)) (fromDynamic d)
  where
    expected = show (typeOf (undefined :: a))

emptyGraph :: Graph s
emptyGraph = Graph mempty 0

getQuantum :: MonadIO m => Vertex -> m Quantum
getQuantum v = liftIO . readIORef $ v^.vertexQuantum

-- returns altered quantum
modifyQuantum :: (MonadIO m, Alternative m) => Vertex -> (Quantum -> (Quantum, a)) -> m a
modifyQuantum v f = do
    (a, before) <- liftIO $ atomicModifyIORef' (v ^. vertexQuantum)
      $ \q ->
          let (newQ, a) = f q
           in (newQ, (a, q))
    return a <|> (liftIO (writeIORef (v^.vertexQuantum) before) *> empty)

edgesFrom :: MonadIO m => Vertex -> m [(Vertex, DFilter)]
edgesFrom v = liftIO . readIORef $ v^.vertexEdges

addEdge :: MonadIO m => Vertex -> Vertex -> DFilter -> m ()
addEdge from' to' dfilter = do
    liftIO $ modifyIORef (from'^.vertexEdges) ((to', dfilter):)

setQuantum :: MonadIO m => Vertex -> Quantum -> m ()
setQuantum v q = liftIO $ writeIORef (v^.vertexQuantum) q

-- valueAt :: Vertex -> Lens' (Graph s) Quantum
-- valueAt (Vertex n) = singular (vertices . ix n . _1)
-- {-# INLINE valueAt #-}

imAsList :: Iso' (IM.IntMap v) [(VertexID, v)]
imAsList = iso IM.toList IM.fromList
{-# INLINABLE imAsList #-}

-- edges :: Vertex -> Lens' (Graph s) (IM.IntMap DFilter)
-- edges (Vertex n) = singular (vertices . ix n . _2)
-- {-# INLINABLE edges #-}

-- edgeBetween :: Vertex -> Vertex -> Lens' (Graph s) (Maybe DFilter)
-- edgeBetween from' (Vertex to') = edges from' . at to'
-- {-# INLINABLE edgeBetween #-}

vertices :: IndexedTraversal' VertexID (Graph s) Vertex
vertices = verticesMap . itraversed
{-# INLINABLE vertices #-}

-- edgesFrom :: Vertex -> Traversal' (Graph s) (Vertex, DFilter)
-- edgesFrom n = edges n . imAsList . traversed . coerced
-- {-# INLINE edgesFrom #-}

entropyOfQ :: Quantum -> (Maybe Int)
entropyOfQ (Quantum (Unknown xs)) = Just $ olength xs
entropyOfQ _ = Nothing
