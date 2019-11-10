{-# LANGUAGE ScopedTypeVariables #-}
module Props.Internal.Links
    ( disjoint
    , equal
    , require
    ) where

import qualified Data.Set as S
import Props.Internal.GraphM
import Data.Typeable

disjoint :: forall a. (Typeable a, Ord a) => PVar (S.Set a) -> PVar (S.Set a) -> GraphM ()
disjoint a b = do
    link a b disj
    link b a disj
  where
    disj :: a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs

equal :: forall a. (Typeable a, Ord a) => PVar (S.Set a) -> PVar (S.Set a) -> GraphM ()
equal a b = do
    link a b eq
    link b a eq
  where
    eq :: a -> S.Set a -> S.Set a
    eq x xs | x `S.member` xs =  S.singleton x
            | otherwise = S.empty

require :: forall a b. (Typeable a, Ord a, Typeable b) => (a -> b -> Bool) -> PVar (S.Set a) -> PVar (S.Set b) -> GraphM ()
require f a b = do
    link a b (S.filter . f)
