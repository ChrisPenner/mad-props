{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WFC where

import Control.Monad.Logic
import Control.Applicative
import Data.Foldable
import System.Random.Shuffle
import Control.Monad.State
import qualified MinTracker as MT

-- Note; State on the OUTSIDE means it WILL backtrack state.
newtype WFC a = WFC (StateT MT.MinTracker (LogicT IO) a)
    deriving newtype (Functor, Alternative, Applicative, Monad, MonadState MT.MinTracker, MonadIO)


guard' :: Bool -> WFC ()
guard' b = WFC . lift . lift $ guard b

backtrack :: WFC a
backtrack = fail "backtrack!"

rselect :: (Foldable f, Functor f) => f a -> WFC a
rselect (toList -> fa) = liftIO (shuffleM fa) >>= select

select :: (Foldable f, Functor f) => f a -> WFC a
select fa = asum (pure <$> fa)

runWFC :: MT.MinTracker -> WFC a -> IO a
runWFC mt (WFC wfc) = observeT . (flip evalStateT mt) $ wfc
