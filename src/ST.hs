module ST where

import Control.Monad.ST
import Control.Monad.Cont
import Data.STRef
import Data.Functor
import Control.Applicative
import Control.Monad.Logic
import Data.Foldable

writeVar :: STRef s a -> a -> LogicT (ST s) ()
writeVar s a = do
    before <- lift $ readSTRef s
    lift $ writeSTRef s a
    pure () <|> (lift $ writeSTRef s before)

choice :: [a] -> STRef s a -> LogicT (ST s) ()
choice as s = asum (writeVar s <$> as)

test :: LogicT (ST s) (Int, Int)
test = do
    a <- lift $ newSTRef 0
    b <- lift $ newSTRef 0
    choice [1, 2, 3, 4] a
    choice [1, 2, 3, 4] b
    a' <- lift $ readSTRef a
    b' <- lift $ readSTRef b
    guard ((a' + b') == 7)
    return (a', b')

main :: IO ()
main = do
    r <- stToIO $ observeAllT test
    print r
