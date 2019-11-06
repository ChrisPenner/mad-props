{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module WFC.Internal.GraphM (GraphM, newPVar, link, solveGraph, readPVar, PVar) where

import WFC.Internal.Graph
import WFC.Internal.WFC
import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.Dynamic
import Data.Typeable.Lens
import Data.MonoTraversable
import Data.Maybe

newtype GraphM s a =
    GraphM { runGraphM :: StateT (Graph s) IO a
           }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

data PVar s f = PVar Vertex
  deriving (Eq, Show, Ord)

newPVar :: (MonoFoldable f, Typeable f, Typeable (Element f)) => f -> GraphM s (PVar s f)
newPVar xs = GraphM $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar (Vertex v))

link :: (Typeable g, Typeable (Element f)) => PVar s f -> PVar s g -> (Element f -> g -> g) -> GraphM s ()
link (PVar from') (PVar to') f = GraphM $ do
    edgeBetween from' to' ?= toDyn f

readPVar :: Typeable a => Graph s -> PVar s f -> a
readPVar g (PVar v) =
    fromMaybe (error "readPVar called on unsolved graph")
    $ (g ^? valueAt v . folding unpackQuantum)

unpackQuantum :: Typeable a => Quantum -> Maybe a
unpackQuantum (Quantum o) = o ^? _Observed . _cast

buildGraph :: GraphM s a -> IO (a, Graph s)
buildGraph = flip runStateT emptyGraph . runGraphM

solveGraph :: GraphM s a -> IO (a, Graph s)
solveGraph m = do
    (a, g) <- buildGraph m
    g' <- solve g
    return (a, g')
