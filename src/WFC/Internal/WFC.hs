{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module WFC.Internal.WFC (solve, debugStepper) where

import qualified WFC.Internal.Graph as G
import Control.Lens hiding (Context)
import WFC.Internal.Backtracking
import WFC.Internal.Graph
import Control.Monad.IO.Class
import qualified WFC.Internal.MinTracker as MT
import Control.Monad
import Data.Dynamic
import Data.MonoTraversable

solve :: Graph s
      -> IO (Graph s)
solve graph' = runWFC mt (solve' graph')
  where
    solve' gr = step gr >>= \case
        Left done -> return done
        Right gr' -> solve' gr'
    mt = initMinTracker graph'

step :: Graph s
     -> Backtrack (Either (Graph s) (Graph s))
step graph' = MT.popMinNode >>= \case
    Nothing -> return $ Left graph'
    Just n  -> Right <$> collapse n graph'
{-# INLINE step #-}

collapse :: G.Vertex
         -> Graph s
         -> Backtrack (Graph s)
collapse n g = do
    let focused = g ^?! valueAt n
    choicesOfQ' focused n g
{-# INLINE collapse #-}

choicesOfQ' :: Quantum -> Vertex -> Graph s -> Backtrack (Graph s)
choicesOfQ' (Quantum (Observed{})) _ _ = error "Can't collapse an already collapsed node!"
choicesOfQ' (Quantum (Unknown xs :: SuperPos f)) n g = do
    let options = otoList xs
    choice <- select options
    let picked = g & valueAt n %~ setChoiceQ (Observed choice :: SuperPos f)
    propagate (n, toDyn choice) picked
{-# INLINE choicesOfQ' #-}

debugStepper :: (Graph s -> IO ())
             -> Graph s
             -> IO (Graph s)
debugStepper stepHandler gr = runWFC mt (debugStepper' gr)
  where
    mt = initMinTracker gr
    debugStepper' gr' = do
      liftIO $ stepHandler gr'
      step gr' >>= \case
          Left done -> return done
          Right gr'' -> debugStepper' gr''

initMinTracker :: Graph s -> MT.MinTracker
initMinTracker graph' = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies :: [(G.Vertex, Maybe Int)]
      allEntropies = allNodes & traversed . _2 %~ entropyOfQ
      allNodes :: [(G.Vertex, Quantum)]
      allNodes =  graph' ^.. values


propagate :: forall s. (Vertex, DChoice) -> Graph s -> Backtrack (Graph s)
propagate (from', choice) g = foldM step' g allEdges
    where
      allEdges :: [(Vertex, DFilter)]
      allEdges = g ^.. edgesFrom from'
      step' :: Graph s -> (Vertex, DFilter) -> Backtrack (Graph s)
      step' g' e = propagateSingle choice e g'
{-# INLINE propagate #-}

propagateSingle :: DChoice -> (Vertex, DFilter) -> Graph s -> Backtrack (Graph s)
propagateSingle v (to', dfilter) g = g & valueAt to' %%~ alterQ
  where
    alterQ :: Quantum -> Backtrack Quantum
    alterQ (Quantum (Unknown xs :: SuperPos f)) = do
        let filteredDown = (forceDyn $ dynApp (dynApp dfilter v) (toDyn xs) :: f)
        MT.setNodeEntropy to' (olength filteredDown)
        return $ Quantum (Unknown filteredDown)
    alterQ q = return q
{-# INLINE propagateSingle #-}
