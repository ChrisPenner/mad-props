{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module WFC (solve, debugStepper) where

import qualified WFC.Graph as G
import Control.Lens hiding (Context)
import WFC.Backtracking
import WFC.Types
import WFC.Graph
import Control.Monad.IO.Class
import qualified WFC.MinTracker as MT
import Data.Foldable as F
import Control.Monad
import Data.Dynamic

-- entropyOf :: DSuperPos -> Maybe Int
-- entropyOf dsuper = Just . count $ toDyn length
--   where
--     count :: SuperPos f a -> Int
--     count = length
-- -- entropyOf (forceDyn -> (Observed _ :: SuperPos f a)) = Nothing

solve :: G.Graph
      -> IO (G.Graph)
solve graph' = runWFC mt (solve' graph')
  where
    solve' gr = step gr >>= \case
        Left done -> return done
        Right gr' -> solve' gr'
    mt = initMinTracker graph'

step :: G.Graph
     -> Backtrack (Either (G.Graph) (G.Graph))
step graph' = MT.popMinNode >>= \case
    Nothing -> return $ Left graph'
    Just n  -> do
        newGraph <- collapse n graph'
        return $ Right newGraph

collapse :: G.Vertex
         -> G.Graph
         -> Backtrack (G.Graph)
collapse n g = do
    let focused = g ^?! valueAt n
    choicesOfQ' focused n g

choicesOfQ' :: Quantum -> Vertex -> Graph -> Backtrack (Graph)
choicesOfQ' (Quantum (Unknown xs :: SuperPos f a)) n g = do
    let options = toList xs
    choice <- rselect options
    let picked = g & valueAt n %~ setChoiceQ (Observed choice :: SuperPos f a)
    propagate (n, toDyn choice) picked

debugStepper :: (G.Graph -> IO ())
             -> G.Graph
             -> IO (G.Graph)
debugStepper stepHandler gr = runWFC mt (debugStepper' gr)
  where
    mt = initMinTracker gr
    debugStepper' gr' = do
      liftIO $ stepHandler gr'
      step gr' >>= \case
          Left done -> return done
          Right gr'' -> debugStepper' gr''

initMinTracker :: G.Graph -> MT.MinTracker
initMinTracker graph' = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies :: [(G.Vertex, Maybe Int)]
      allEntropies = allNodes & traversed . _2 %~ entropyOfQ
      allNodes :: [(G.Vertex, Quantum)]
      allNodes =  graph' ^@.. values


propagate :: (Vertex, DChoice) -> Graph -> Backtrack (Graph)
propagate (from', choice) g = foldM step' g allEdges
    where
      allEdges :: [(Vertex, DFilter)]
      allEdges = g ^.. edgesFrom from'
      step' :: Graph -> (Vertex, DFilter) -> Backtrack (Graph)
      step' g' e = propagateSingle choice e g'
{-# INLINABLE propagate #-}

propagateSingle :: DChoice -> (Vertex, DFilter) -> Graph -> Backtrack Graph
propagateSingle v (to', dfilter) g = g & valueAt to' %%~ alterQ
  where
    alterQ :: Quantum -> Backtrack Quantum
    alterQ (Quantum (Unknown xs :: SuperPos f a)) = do
        let filteredDown = (forceDyn $ dynApp (dynApp dfilter v) (toDyn xs) :: f a)
        MT.setNodeEntropy to' (length filteredDown)
        return $ Quantum (Unknown filteredDown)
    alterQ q = return q
{-# INLINABLE propagateSingle #-}

