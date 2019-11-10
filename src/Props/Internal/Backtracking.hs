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

-- Note; State on the OUTSIDE means it WILL backtrack state.
newtype Backtrack a = Backtrack (StateT BState (LogicT IO) a)
    deriving newtype (Functor, Alternative, Applicative, Monad, MonadState BState, MonadIO)

data BState =
    BState { _bsMinTracker :: MT.MinTracker
           , _graph      :: Graph
           }
makeLenses ''BState

instance MT.HasMinTracker BState where
  minTracker = bsMinTracker

rselect :: (Foldable f, Functor f) => f a -> Backtrack a
rselect (toList -> fa) = liftIO (shuffleM fa) >>= select
{-# INLINE rselect #-}

select :: (Foldable f, Functor f) => f a -> Backtrack a
select fa = asum (pure <$> fa)
{-# INLINE select #-}

runBacktrack :: MT.MinTracker -> Graph -> Backtrack a -> IO (a, Graph)
runBacktrack mt g (Backtrack wfc) = fmap (second _graph) . observeT . (flip runStateT (BState mt g)) $ wfc

runBacktrackAll :: MT.MinTracker -> Graph -> Backtrack a -> IO [(a, Graph)]
runBacktrackAll mt g (Backtrack wfc) = observeAllT . fmap (second _graph) . (flip runStateT (BState mt g)) $ wfc
