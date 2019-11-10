{-# LANGUAGE ScopedTypeVariables #-}
module ST.Internal.Links
    ( disjoint
    , equal
    , require
    ) where

import qualified Data.Set as S
import ST.Internal.GraphM
import Data.Typeable

disjoint :: forall s a. (Typeable a, Ord a) => PVar s (S.Set a) -> PVar s (S.Set a) -> GraphM s ()
disjoint a b = do
    link a b disj
    link b a disj
  where
    disj :: a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs

equal :: forall s a. (Typeable a, Ord a) => PVar s (S.Set a) -> PVar s (S.Set a) -> GraphM s ()
equal a b = do
    link a b eq
    link b a eq
  where
    eq :: a -> S.Set a -> S.Set a
    eq x xs | x `S.member` xs =  S.singleton x
            | otherwise = S.empty

require :: forall s a b. (Typeable a, Ord a, Typeable b) => (a -> b -> Bool) -> PVar s (S.Set a) -> PVar s (S.Set b) -> GraphM s ()
require f a b = do
    link a b (S.filter . f)
