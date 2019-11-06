{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WFC.GraphM where

import WFC.Graph
import Control.Monad.State
import Control.Lens
import WFC.Types
import Data.Typeable
import Data.Dynamic
import Unsafe.Coerce

newtype GraphM a = GraphM {runGraphM :: StateT Graph IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)

data PVar (f :: * -> *) a = PVar Vertex
  deriving (Eq, Show, Ord)

newPVar :: Typeable a => [a] -> GraphM (PVar [] a)
newPVar xs = GraphM $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar v)

link :: (Typeable a, Typeable g, Typeable b) => PVar f a -> PVar g b -> (a -> g b -> g b) -> GraphM ()
link (PVar from') (PVar to') f = GraphM $ do
    edges from' . at to' ?= toDyn f

getGraph :: GraphM Graph
getGraph = GraphM get

readPVar :: PVar f a -> Graph -> a
readPVar (PVar v) g = g ^?! valueAt v . to unpackQuantum

unpackQuantum :: Quantum -> a
unpackQuantum (Quantum (Observed a)) = unsafeCoerce a

buildGraph :: GraphM a -> IO Graph
buildGraph = flip execStateT emptyGraph . runGraphM
