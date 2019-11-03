{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WFC where

import Control.Monad.Cont
import Control.Applicative
import Data.Foldable
import Text.Printf
import System.Random.Shuffle
import Control.Monad.State
import qualified MinTracker as MT

newtype WFC a = WFC (StateT MT.MinTracker (ContT () IO) a)
    deriving newtype (Functor, Applicative, Monad, MonadState MT.MinTracker, MonadCont, MonadIO)

guard' :: Bool -> WFC ()
guard' b = WFC . lift . lift $ guard b

backtrack :: WFC a
backtrack = WFC . lift . lift $ fail "backtrack!"

rselect :: (Foldable f, Functor f) => f a -> WFC a
rselect (toList -> fa) = liftIO (shuffleM fa) >>= select

rselectWithTrigger :: (Foldable f, Functor f) => (a -> IO ()) -> f a -> WFC a
rselectWithTrigger trig (toList -> fa) = (liftIO (shuffleM fa)) >>= selectWithTrigger trig

select :: (Foldable f, Functor f) => f a -> WFC a
select fa = WFC . lift . ContT $ \k -> asum (k <$> fa)

selectWithTrigger :: (Foldable f, Functor f) => (a -> IO ()) -> f a -> WFC a
selectWithTrigger trig (toList -> (x:xs)) =
    WFC . lift . ContT $ \k -> k x <|> asum ((\a -> trig a *> k a) <$> xs)
selectWithTrigger _ fa = WFC . lift . ContT $ \k -> asum (k <$> fa)

runWFC :: MT.MinTracker -> WFC () -> IO ()
runWFC mt wfc = runWFC' mt wfc return

runWFC' :: MT.MinTracker -> WFC a -> (a -> IO ()) -> IO ()
runWFC' mt (WFC wfc) h = flip runContT h . (flip evalStateT mt) $ wfc


backtracktest :: IO ()
backtracktest = runWFC MT.empty $ do
    n <- selectWithTrigger (const $ print "backing up!") [1 :: Int, 2, 3]
    n' <- selectWithTrigger (printf "nonono: %d\n") [1 :: Int, 2, 3]
    liftIO $ printf "trying: (%d, %d)\n" n n'
    guard' (n + n' == 5)
    liftIO $ printf "Success: %d + %d = 5\n" n n'

-- >>> test
-- trying: (1, 1)
-- trying: (1, 2)
-- trying: (1, 3)
-- trying: (2, 1)
-- trying: (2, 2)
-- trying: (2, 3)
-- Success: 2 + 3 = 5

select' :: (MonadCont m, Alternative m, Foldable f, Functor f) => f a -> m a
select' fa = callCC $ \k -> asum (k <$> fa)
