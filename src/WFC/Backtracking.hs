{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WFC.Backtracking where

import Control.Monad.Logic
import Control.Applicative
import Data.Foldable
import System.Random.Shuffle
import Control.Monad.State
import qualified MinTracker as MT

-- Note; State on the OUTSIDE means it WILL backtrack state.
newtype Backtrack a = Backtrack (StateT MT.MinTracker (LogicT IO) a)
    deriving newtype (Functor, Alternative, Applicative, Monad, MonadState MT.MinTracker, MonadIO)


guard' :: Bool -> Backtrack ()
guard' b = Backtrack . lift . lift $ guard b

backtrack :: Backtrack a
backtrack = fail "backtrack!"

rselect :: (Foldable f, Functor f) => f a -> Backtrack a
rselect (toList -> fa) = liftIO (shuffleM fa) >>= select

select :: (Foldable f, Functor f) => f a -> Backtrack a
select fa = asum (pure <$> fa)

runWFC :: MT.MinTracker -> Backtrack a -> IO a
runWFC mt (Backtrack wfc) = observeT . (flip evalStateT mt) $ wfc
