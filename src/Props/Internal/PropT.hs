{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Props.Internal.PropT
    ( Prop
    , PropT
    , newPVar
    , constrain
    , solve
    , solveAll
    , readPVar
    , PVar
    ) where

import Props.Internal.Graph
import qualified Props.Internal.Props as P
import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.Dynamic
import Data.Maybe

-- | Pure version of 'PropT'
type Prop a = PropT Identity a


{-|
A monad transformer for setting up constraint problems.
-}
newtype PropT m a =
    PropT { runGraphM :: StateT Graph m a
           }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)

{-|
A propagator variable where the possible values @a@ are contained in the container @f@.
-}
data PVar (f :: * -> *) a where
  PVar :: (Typeable a, Typeable f) => Vertex -> PVar f a

-- | Nominal equality, Ignores contents
instance Eq (PVar f a) where
  (PVar v) == (PVar t) = v == t

instance Ord (PVar f a) where
  PVar v <= PVar t = v <= t

instance Show (PVar f a) where
  show (PVar _) = unwords ["PVar", show (typeRep (Proxy @f)), show (typeRep (Proxy @a))]

{-|
Used to create a new propagator variable within the setup for your problem.

@f@ is any Foldable container which contains each of the possible states which the variable could take.

E.g. For a sudoku solver you would use 'newPVar' to create a variable for each cell, passing a @Set Int@ containing the numbers @[1..9]@.
-}
newPVar :: (Monad m, Foldable f, Typeable f, Typeable a) => f a -> PropT m (PVar f a)
newPVar xs = PropT $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar (Vertex v))

{-|
'constrain' the relationship between two 'PVar's. Note that this is a ONE WAY relationship; e.g. @constrain a b f@ will propagate constraints from @a@ to @b@ but not vice versa.

Given @PVar f a@ and @PVar g b@ as arguments, provide a function which will filter/alter the options in @g@ according to the choice.

For a sudoku puzzle you'd have two @Pvar Set Int@'s, each representing a cell on the board.
You can constrain @b@ to be a different value than @a@ with the following call:

> constrain a b $ \elementA setB -> S.delete elementA setB)

Take a look at some linking functions which are already provided: 'disjoint', 'equal', 'require'
-}
constrain :: Monad m
          => PVar f a
          -> PVar g b
          -> (a -> g b -> g b)
          -> PropT m ()
constrain (PVar from') (PVar to') f = PropT $ do
    edgeBetween from' to' ?= toDyn f

readPVar :: Graph -> PVar f a -> a
readPVar g (PVar v) =
    fromMaybe (error "readPVar called on unsolved graph")
    $ (g ^? valueAt v . folding unpackQuantum)

unpackQuantum :: (Typeable a) => Quantum -> Maybe a
unpackQuantum (Quantum (Observed xs)) = cast xs
unpackQuantum (Quantum _) = Nothing

buildGraph :: PropT m a -> m (a, Graph)
buildGraph = flip runStateT emptyGraph . runGraphM

{-|
Provide an initialization action which constrains the problem, and a finalizer, and 'solveT' will return a result if one exists.

The finalizer is an annoyance caused by the fact that GHC does not yet support Impredicative Types.

For example, if you wrote a solution to the nQueens problem, you might run it like so:

> -- Set up the problem for 'n' queens and return their PVar's as a list.
> initNQueens :: Int -> Prop [PVar S.Set Coord]
> initNQueens = ...
>
> solution :: [Coord]
> solution = solve (initNQueens 8) (\readPVar vars -> fmap readPVar vars)
which converts 'PVar's into a result.Given an action which initializes and constrains a problem 'solveT' will  and returns some container of 'PVar's, 'solveT' will attempt to find a solution which passes all valid constraints.
-}
solveT :: forall m a r.
       Monad m
       => ((forall f x. PVar f x -> x) -> a -> r)
       -> PropT m a
       -> m (Maybe r)
solveT f m = do
    (a, g) <- buildGraph m
    case P.solve g of
        Nothing -> return Nothing
        Just solved -> return . Just $ f (readPVar solved) a


{-|
Like 'solveT', but finds ALL possible solutions. There will likely be duplicates.
-}
solveAllT :: forall m a r.
          Monad m
          => ((forall f x. PVar f x -> x) -> a -> r)
          -> PropT m a
          -> m [r]
solveAllT f m = do
    (a, g) <- buildGraph m
    let gs = P.solveAll g
    return $ gs <&> \g' -> f (readPVar g') a

{-|
Pure version of 'solveT'
-}
solve :: forall a r.
        ((forall f x. PVar f x -> x) -> a -> r)
      -> Prop a
      -> (Maybe r)
solve f = runIdentity . solveT f

{-|
Pure version of 'solveAllT'
-}
solveAll :: forall a r.
            ((forall f x. PVar f x -> x) -> a -> r)
          -> Prop a
          -> [r]
solveAll f = runIdentity . solveAllT f
