{-# LANGUAGE ViewPatterns #-}
module Backtrack where

import Control.Monad.Cont
import Control.Applicative
import Data.Foldable
import Text.Printf
import System.Random.Shuffle

type Backtrack a = ContT () IO a

rselect :: (Foldable f, Functor f) => f a -> Backtrack a
rselect (toList -> fa) = shuffleM fa >>= select

rselectWithTrigger :: (Foldable f, Functor f) => (a -> IO ()) -> f a -> Backtrack a
rselectWithTrigger trig (toList -> fa) = shuffleM fa >>= selectWithTrigger trig

select :: (Foldable f, Functor f) => f a -> Backtrack a
select fa = ContT $ \k -> asum (k <$> fa)

selectWithTrigger :: (Foldable f, Functor f) => (a -> IO ()) -> f a -> Backtrack a
selectWithTrigger trig (toList -> (x:xs)) =
    ContT $ \k -> k x <|> asum ((\a -> trig a *> k a) <$> xs)
selectWithTrigger _ fa = ContT $ \k -> asum (k <$> fa)

runBacktrack :: Backtrack () -> IO ()
runBacktrack = flip runContT return

runBacktrack' :: Backtrack a -> (a -> IO ()) -> IO ()
runBacktrack' = runContT


backtracktest :: IO ()
backtracktest = flip runContT return $ do
    n <- selectWithTrigger (const $ print "backing up!") [1 :: Int, 2, 3]
    n' <- selectWithTrigger (printf "nonono: %d\n") [1 :: Int, 2, 3]
    liftIO $ printf "trying: (%d, %d)\n" n n'
    lift $ guard (n + n' == 5)
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
