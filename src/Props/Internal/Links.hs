{-# LANGUAGE ScopedTypeVariables #-}
module Props.Internal.Links
    ( disjoint
    , equal
    , require
    ) where

import qualified Data.Set as S
import Props.Internal.PropT

{-|
Apply the constraint that two variables may NOT be set to the same value. This constraint is bidirectional.

E.g. you might apply this constraint to two cells in the same row of sudoku grid to assert they don't contain the same value.
-}
disjoint :: forall a m. (Monad m, Ord a) => PVar S.Set a -> PVar S.Set a -> PropT m ()
disjoint a b = do
    constrain a b disj
    constrain b a disj
  where
    disj :: a -> S.Set a -> S.Set a
    disj x xs = S.delete x xs

{-|
Apply the constraint that two variables MUST be set to the same value. This constraint is bidirectional.
-}
equal :: forall a m. (Monad m, Ord a) => PVar S.Set a -> PVar S.Set a -> PropT m ()
equal a b = do
    constrain a b eq
    constrain b a eq
  where
    eq :: a -> S.Set a -> S.Set a
    eq x xs | x `S.member` xs =  S.singleton x
            | otherwise = S.empty

{-|
Given a choice for @a@; filter for valid options of @b@ using the given predicate.

E.g. if @a@ must always be greater than @b@, you could require:

> require (>) a b
-}
require :: Monad m => (a -> b -> Bool) -> PVar S.Set a -> PVar S.Set b -> PropT m ()
require f a b = do
    constrain a b (S.filter . f)
