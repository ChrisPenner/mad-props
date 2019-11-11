{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Props.Internal.PropT
    ( Prop
    , PropT
    , newPVar
    , link
    , solve
    , solveAll
    , readPVar
    , PVar
    ) where

import Props.Internal.Graph
import qualified Props.Internal.Props as P
import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.Dynamic
import Data.MonoTraversable
import Data.Maybe

type Prop a = PropT Identity a
newtype PropT m a =
    PropT { runGraphM :: StateT Graph m a
           }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

data PVar f = PVar Vertex
  deriving (Eq, Show, Ord)

newPVar :: (Monad m, MonoFoldable f, Typeable f, Typeable (Element f)) => f -> PropT m (PVar f)
newPVar xs = PropT $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar (Vertex v))

link :: (Monad m, Typeable g, Typeable (Element f)) => PVar f -> PVar g -> (Element f -> g -> g) -> PropT m ()
link (PVar from') (PVar to') f = PropT $ do
    edgeBetween from' to' ?= toDyn f

readPVar :: (Typeable (Element f)) => Graph -> PVar f -> Element f
readPVar g (PVar v) =
    fromMaybe (error "readPVar called on unsolved graph")
    $ (g ^? valueAt v . folding unpackQuantum)

unpackQuantum :: (Typeable a) => Quantum -> Maybe a
unpackQuantum (Quantum (Observed xs)) = cast xs
unpackQuantum (Quantum _) = Nothing

buildGraph :: (Monad m) => PropT m a -> m (a, Graph)
buildGraph = flip runStateT emptyGraph . runGraphM

solveT :: (Monad m, Functor f, Typeable (Element g)) => PropT m (f (PVar g)) -> m (f (Element g))
solveT m = do
    (a, g) <- buildGraph m
    let solved = P.solve g
    return (fmap (readPVar solved) a)

solveAllT :: (Monad m, Functor f, Typeable (Element g)) => PropT m (f (PVar g)) -> m ([f (Element g)])
solveAllT m = do
    (fa, g) <- buildGraph m
    let gs = P.solveAll g
    return $ gs <&> \g' -> (readPVar g') <$> fa

solve :: (Functor f, Typeable (Element g)) => Prop (f (PVar g)) -> f (Element g)
solve = runIdentity . solveT
solveAll :: (Functor f, Typeable (Element g)) => Prop (f (PVar g)) -> [f (Element g)]
solveAll = runIdentity . solveAllT
