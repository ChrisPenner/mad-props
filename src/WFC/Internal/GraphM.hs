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
import WFC.Internal.Types
import Data.Typeable
import Data.Dynamic
import Data.Typeable.Lens

newtype GraphM s a =
    GraphM { runGraphM :: StateT (Graph s) IO a
           }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

data PVar s (f :: * -> *) a = PVar Vertex
  deriving (Eq, Show, Ord)

newPVar :: Typeable a => [a] -> GraphM s (PVar s [] a)
newPVar xs = GraphM $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar (Vertex v))

link :: (Typeable a, Typeable g, Typeable b) => PVar s f a -> PVar s g b -> (a -> g b -> g b) -> GraphM s ()
link (PVar from') (PVar to') f = GraphM $ do
    edgeBetween from' to' ?= toDyn f

readPVar :: Typeable a => Graph s -> PVar s f a -> a
readPVar g (PVar v) = (g ^?! valueAt v . to unpackQuantum)

unpackQuantum :: Typeable a => Quantum -> a
unpackQuantum (Quantum o) = o ^?! _Observed . _cast

buildGraph :: GraphM s a -> IO (a, Graph s)
buildGraph = flip runStateT emptyGraph . runGraphM

solveGraph :: GraphM s a -> IO (a, Graph s)
solveGraph m = do
    (a, g) <- buildGraph m
    g' <- solve g
    return (a, g')
