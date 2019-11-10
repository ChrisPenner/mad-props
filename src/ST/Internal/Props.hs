{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module ST.Internal.Props (solve) where

import qualified ST.Internal.Graph as G
import Control.Lens hiding (Context)
import ST.Internal.Backtracking
import ST.Internal.Graph
import qualified ST.Internal.MinTracker as MT
import Data.Dynamic
import Data.MonoTraversable
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Functor

solve :: Graph s
      -> IO ()
solve graph' = do
    mt <- initMinTracker graph'
    runBacktrack mt solve'
  where
    solve' = step >>= \case
        Done -> return ()
        NotDone -> solve'

-- solveAll :: Graph s
--       -> IO [Graph s]
-- solveAll graph' = do
--     mt <- initMinTracker graph'
--     runBacktrackAll mt (solve' graph')
--   where
--     solve' gr = step gr >>= \case
--         Done -> return ()
--         NotDone -> solve'


data Finished = Done | NotDone
step :: Backtrack Finished
step = MT.popMinNode >>= \case
    Nothing -> return Done
    Just n  -> collapse n $> NotDone
{-# INLINE step #-}

collapse :: G.Vertex -> Backtrack ()
collapse v = do
    getQuantum v >>= \case
      (Quantum (Observed{})) -> error "Can't collapse an already collapsed node!"
      before@(Quantum (Unknown xs :: SuperPos f)) -> do
        let options = otoList xs
        choice <- select options <|> (setQuantum v before *> backtrack)
        setQuantum v (Quantum (Observed choice :: SuperPos f))
        propagate (v, toDyn choice)
{-# INLINE collapse #-}


initMinTracker :: Graph s -> IO MT.MinTracker
initMinTracker g = do
    ents <- allEntropies
    return $ MT.fromList (ents ^.. traversed . below _Just)
    where
      allEntropies :: IO [(G.Vertex, Maybe Int)]
      allEntropies = for allNodes nodeEntropy
      nodeEntropy :: Vertex -> IO (Vertex, Maybe Int)
      nodeEntropy v = do
          q <- getQuantum v
          return (v, entropyOfQ q)
      allNodes :: [G.Vertex]
      allNodes =  g ^.. vertices


propagate :: (Vertex, DChoice) -> Backtrack ()
propagate (from', choice) = do
    allEdges <- edgesFrom from'
    for_ allEdges step'
    where
      step' :: (Vertex, DFilter) -> Backtrack ()
      step' e = propagateSingle choice e
{-# INLINE propagate #-}

propagateSingle :: DChoice -> (Vertex, DFilter) -> Backtrack ()
propagateSingle choice (to', dfilter) = do
    modifyQuantum to' alterQ >>= \case
      Nothing -> return ()
      Just newEntropy -> do
          MT.setNodeEntropy to' newEntropy
  where
    alterQ :: Quantum -> (Quantum, Maybe Int)
    alterQ (Quantum (Unknown xs :: SuperPos f)) = do
        let filteredDown = (forceDyn $ dynApp (dynApp dfilter choice) (toDyn xs) :: f)
         in (Quantum (Unknown filteredDown), Just $ olength filteredDown)
    alterQ q = (q, Nothing)
{-# INLINE propagateSingle #-}
