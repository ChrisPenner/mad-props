{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WFC.Internal.Backtracking where

import Control.Monad.Logic
import Control.Applicative
import Data.Foldable
import System.Random.Shuffle
import Control.Monad.State
import qualified WFC.Internal.MinTracker as MT

-- Note; State on the OUTSIDE means it WILL backtrack state.
newtype Backtrack a = Backtrack (StateT MT.MinTracker (LogicT IO) a)
    deriving newtype (Functor, Alternative, Applicative, Monad, MonadState MT.MinTracker, MonadIO)

rselect :: (Foldable f, Functor f) => f a -> Backtrack a
rselect (toList -> fa) = liftIO (shuffleM fa) >>= select
{-# INLINE rselect #-}

select :: (Foldable f, Functor f) => f a -> Backtrack a
select fa = asum (pure <$> fa)
{-# INLINE select #-}

runWFC :: MT.MinTracker -> Backtrack a -> IO a
runWFC mt (Backtrack wfc) = observeT . (flip evalStateT mt) $ wfc
