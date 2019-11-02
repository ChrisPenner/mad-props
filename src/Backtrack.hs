{-# LANGUAGE ViewPatterns #-}
module Backtrack where

import Control.Monad.Cont
import Control.Applicative
import Data.Foldable
import Text.Printf

select :: (Alternative m, Foldable f, Functor f) => f a -> ContT r m a
select fa = ContT $ \k -> asum (k <$> fa)

selectWithTrigger :: (Alternative m, Foldable f, Functor f) => (a -> m ()) -> f a -> ContT r m a
selectWithTrigger trig (toList -> (x:xs)) =
    ContT $ \k -> k x <|> asum ((\a -> trig a *> k a) <$> xs)
selectWithTrigger _ fa = ContT $ \k -> asum (k <$> fa)


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
