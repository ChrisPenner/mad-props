{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.MonoTraversable
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
A propagator variable where the possible values are contained in the MonoFoldable type @f@.
-}
data PVar f = PVar Vertex
  deriving (Eq -- ^ Nominal equality, Ignores contents
    , Ord -- ^ Nominal ordering, Ignores contents.
    , Show
           )

{-|
Used to create a new propagator variable within the setup for your problem.

@f@ is any MonoFoldable container which contains each of the possible states which the variable could take. In practice this most standard containers make a good candidate, it's easy to define a your own instance if needed.

E.g. For a sudoku solver you would use 'newPVar' to create a variable for each cell, passing a @Set Int@ or @IntSet@ containing the numbers @[1..9]@.
-}
newPVar :: (Monad m, MonoFoldable f, Typeable f, Typeable (Element f)) => f -> PropT m (PVar f)
newPVar xs = PropT $ do
    v <- vertexCount <+= 1
    vertices . at v ?= (Quantum (Unknown xs), mempty)
    return (PVar (Vertex v))

{-|
'constrain' the relationship between two 'PVar's. Note that this is a ONE WAY relationship; e.g. @constrain a b f@ will propagate constraints from @a@ to @b@ but not vice versa.

Given @PVar f@ and @PVar g@ as arguments, provide a function which will filter/alter the options in @g@ according to the choice.

For a sudoku puzzle @f@ and @g@ each represent cells on the board. If @f ~ Set Int@ and @g ~ Set Int@, then you might pass a constraint filter:

> constrain a b $ \elementA setB -> S.delete elementA setB)

Take a look at some linking functions which are already provided: 'disjoint', 'equal', 'require'
-}
constrain :: (Monad m, Typeable g, Typeable (Element f)) => PVar f -> PVar g -> (Element f -> g -> g) -> PropT m ()
constrain (PVar from') (PVar to') f = PropT $ do
    edgeBetween from' to' ?= toDyn f

readPVar :: (Typeable (Element f)) => Graph -> PVar f -> Element f
readPVar g (PVar v) =
    fromMaybe (error "readPVar called on unsolved graph")
    $ (g ^? valueAt v . folding unpackQuantum)

unpackQuantum :: (Typeable a) => Quantum -> Maybe a
unpackQuantum (Quantum (Observed xs)) = cast xs
unpackQuantum (Quantum _) = Nothing

buildGraph :: (Monad m) => PropT m a -> m (a, Graph)
buildGraph = flip runStateT emptyGraph . runGraphM

{-|
Given an action which initializes and constrains a problem and returns some container of 'PVar's, 'solveT' will attempt to find a solution which passes all valid constraints.
-}
solveT :: (Monad m, Functor f, Typeable (Element g)) => PropT m (f (PVar g)) -> m (Maybe (f (Element g)))
solveT m = do
    (a, g) <- buildGraph m
    case P.solve g of
        Nothing -> return Nothing
        Just solved -> return . Just $ readPVar solved <$> a

{-|
Like 'solveT', but finds ALL possible solutions. There will likely be duplicates.
-}
solveAllT :: (Monad m, Functor f, Typeable (Element g)) => PropT m (f (PVar g)) -> m ([f (Element g)])
solveAllT m = do
    (fa, g) <- buildGraph m
    let gs = P.solveAll g
    return $ gs <&> \g' -> (readPVar g') <$> fa

{-|
Pure version of 'solveT'
-}
solve :: (Functor f, Typeable (Element g)) => Prop (f (PVar g)) -> Maybe (f (Element g))
solve = runIdentity . solveT

{-|
Pure version of 'solveAllT'
-}
solveAll :: (Functor f, Typeable (Element g)) => Prop (f (PVar g)) -> [f (Element g)]
solveAll = runIdentity . solveAllT
