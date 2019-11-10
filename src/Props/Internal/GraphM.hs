{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Props.Internal.GraphM
    ( GraphM
    , newPVar
    , link
    , solveGraph
    , solveGraph'
    , solveGraphAll
    , readPVar
    , PVar
    ) where

import Props.Internal.Graph
import Props.Internal.Props
import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.Dynamic
import Data.MonoTraversable
import Data.Maybe

newtype GraphM a =
    GraphM { runGraphM :: StateT Graph IO a
           }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

data PVar f = PVar Vertex
  deriving (Eq, Show, Ord)

newPVar :: (MonoFoldable f, Typeable f, Typeable (Element f)) => f -> GraphM (PVar f)
newPVar xs = GraphM $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar (Vertex v))

link :: (Typeable g, Typeable (Element f)) => PVar f -> PVar g -> (Element f -> g -> g) -> GraphM ()
link (PVar from') (PVar to') f = GraphM $ do
    edgeBetween from' to' ?= toDyn f

readPVar :: (Typeable (Element f)) => Graph -> PVar f -> Element f
readPVar g (PVar v) =
    fromMaybe (error "readPVar called on unsolved graph")
    $ (g ^? valueAt v . folding unpackQuantum)

unpackQuantum :: (Typeable a) => Quantum -> Maybe a
unpackQuantum (Quantum (Observed xs)) = cast xs
unpackQuantum (Quantum _) = Nothing

buildGraph :: GraphM a -> IO (a, Graph)
buildGraph = flip runStateT emptyGraph . runGraphM

solveGraph' :: (Functor f, Typeable (Element g)) => GraphM (f (PVar g)) -> IO (f (Element g))
solveGraph' m = do
    (a, g) <- buildGraph m
    g' <- solve g
    return (fmap (readPVar g') a)

solveGraph :: GraphM a -> IO (a, Graph)
solveGraph m = do
    (a, g) <- buildGraph m
    g' <- solve g
    return (a, g')

solveGraphAll :: GraphM a -> IO (a, [Graph])
solveGraphAll m = do
    (a, g) <- buildGraph m
    g' <- solveAll g
    return (a, g')
