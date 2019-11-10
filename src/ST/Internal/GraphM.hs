{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ST.Internal.GraphM
    ( GraphM
    , newPVar
    , link
    , solveGraph
    , solveGraphAll
    , readPVar
    , PVar
    ) where

import ST.Internal.Graph
import ST.Internal.Props
import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.Dynamic
import Data.MonoTraversable
import Data.IORef

newtype GraphM s a =
    GraphM { runGraphM :: StateT (Graph s) IO a
           }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

data PVar s f = PVar Vertex
  deriving (Eq, Show, Ord)

newPVar :: (MonoFoldable f, Typeable f, Typeable (Element f)) => f -> GraphM s (PVar s f)
newPVar xs = GraphM $ do
    vID <- vertexCount <+= 1
    v <- liftIO $ Vertex vID <$> newIORef (Quantum (Unknown xs)) <*> newIORef mempty
    verticesMap . at vID ?= v
    return (PVar v)

link :: (Typeable g, Typeable (Element f)) => PVar s f -> PVar s g -> (Element f -> g -> g) -> GraphM s ()
link (PVar from') (PVar to') f = GraphM $ do
    addEdge from' to' (toDyn f)

readPVar :: (MonadIO m, Typeable (Element f)) => PVar s f -> m (Element f)
readPVar (PVar v) = do
    q <- getQuantum v
    case unpackQuantum q of
        Nothing -> (error "readPVar called on unsolved graph")
        Just r -> return r

unpackQuantum :: (Typeable a) => Quantum -> Maybe a
unpackQuantum (Quantum (Observed xs)) = cast xs
unpackQuantum (Quantum _) = Nothing

buildGraph :: GraphM s a -> IO (a, Graph s)
buildGraph = flip runStateT emptyGraph . runGraphM

solveGraph :: GraphM s a -> IO (a, Graph s)
solveGraph m = do
    (a, g) <- buildGraph m
    g' <- solve g
    return (a, g')

solveGraphAll :: GraphM s a -> IO (a, [Graph s])
solveGraphAll m = do
    (a, g) <- buildGraph m
    g' <- solveAll g
    return (a, g')
