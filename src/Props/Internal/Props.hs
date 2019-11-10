{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Props.Internal.Props (solve, solveAll) where

import qualified Props.Internal.Graph as G
import Control.Lens hiding (Context)
import Props.Internal.Backtracking
import Props.Internal.Graph
import qualified Props.Internal.MinTracker as MT
import Data.Dynamic
import Data.MonoTraversable
import Data.Foldable
import Control.Monad.State

solve :: Graph
      -> IO Graph
solve graph' = snd <$> runBacktrack (initMinTracker graph') graph' solve'

solveAll :: Graph -> IO [Graph]
solveAll graph' = fmap snd <$> runBacktrackAll (initMinTracker graph') graph' solve'

solve' :: Backtrack ()
solve' = MT.popMinNode >>= \case
    Nothing -> return ()
    Just n  -> collapse n *> solve'
{-# INLINABLE solve' #-}

collapse :: G.Vertex
         -> Backtrack ()
collapse n = do
    focused <- use (singular (graph . valueAt n))
    choicesOfQ' focused n
{-# INLINE collapse #-}

choicesOfQ' :: Quantum -> Vertex -> Backtrack ()
choicesOfQ' (Quantum (Observed{})) _ = error "Can't collapse an already collapsed node!"
choicesOfQ' (Quantum (Unknown xs :: SuperPos f)) n = do
    let options = otoList xs
    choice <- select options
    graph . valueAt n . superPos .= (Observed choice :: SuperPos f)
    propagate (n, toDyn choice)
{-# INLINE choicesOfQ' #-}

initMinTracker :: Graph -> MT.MinTracker
initMinTracker graph' = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies :: [(G.Vertex, Maybe Int)]
      allEntropies = allNodes ^.. traversed . alongside id (to entropyOfQ)
      allNodes :: [(G.Vertex, Quantum)]
      allNodes =  graph' ^.. values

propagate :: (Vertex, DChoice) -> Backtrack ()
propagate (from', choice) = do
    allEdges <- gets (toListOf (graph . edgesFrom from'))
    for_ allEdges step'
    where
      step' :: (Vertex, DFilter) -> Backtrack ()
      step' e = propagateSingle choice e
{-# INLINE propagate #-}

propagateSingle :: DChoice -> (Vertex, DFilter) -> Backtrack ()
propagateSingle v (to', dfilter) = do
    graph . valueAt to' %%= alterQ >>= \case
      Nothing -> return ()
      Just newEnt -> MT.setNodeEntropy to' newEnt
    return ()
  where
    alterQ :: Quantum -> (Maybe Int, Quantum)
    alterQ (Quantum (Unknown xs :: SuperPos f)) = do
        let filteredDown = (forceDyn $ dynApp (dynApp dfilter v) (toDyn xs) :: f)
         in (Just $ olength filteredDown, Quantum (Unknown filteredDown))
    alterQ q = (Nothing, q)
{-# INLINE propagateSingle #-}
