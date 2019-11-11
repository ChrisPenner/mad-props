{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Props.Internal.Backtracking where

import Control.Monad.Logic
import Control.Applicative
import Data.Foldable
import System.Random.Shuffle
import Control.Monad.State
import Props.Internal.Graph
import qualified Props.Internal.MinTracker as MT
import Control.Lens
import Data.Bifunctor
import System.Random
import Control.Monad.Random
import Data.Maybe

-- Note; State on the OUTSIDE means it WILL backtrack state.
newtype Backtrack a = Backtrack (StateT BState (RandT StdGen Logic) a)
    deriving newtype (Functor, Alternative, Applicative, Monad, MonadState BState, MonadRandom)

data BState =
    BState { _bsMinTracker :: MT.MinTracker
           , _graph      :: Graph
           }
makeLenses ''BState

instance MT.HasMinTracker BState where
  minTracker = bsMinTracker

rselect :: (Foldable f, Functor f) => f a -> Backtrack a
rselect (toList -> fa) = (shuffleM fa) >>= select
{-# INLINE rselect #-}

select :: (Foldable f, Functor f) => f a -> Backtrack a
select fa = asum (pure <$> fa)
{-# INLINE select #-}

runBacktrack :: MT.MinTracker -> Graph -> Backtrack a -> Maybe (a, Graph)
runBacktrack mt g (Backtrack m) =
    fmap (second _graph)
    . listToMaybe
    . observeMany 1
    . flip evalRandT (mkStdGen 0)
    . flip runStateT (BState mt g)
    $ m

runBacktrackAll :: MT.MinTracker -> Graph -> Backtrack a -> [(a, Graph)]
runBacktrackAll mt g (Backtrack m) =
    fmap (second _graph)
    . observeAll
    . flip evalRandT (mkStdGen 0)
    . flip runStateT (BState mt g)
    $ m
