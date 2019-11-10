{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module ST.Internal.Props (solve, debugStepper, solveAll) where

import qualified ST.Internal.Graph as G
import Control.Lens hiding (Context)
import ST.Internal.Backtracking
import ST.Internal.Graph
import Control.Monad.IO.Class
import qualified ST.Internal.MinTracker as MT
import Control.Monad
import Data.Dynamic
import Data.MonoTraversable
import Control.Applicative
import Data.Traversable

solve :: Graph s
      -> IO (Graph s)
solve graph' = do
    mt <- initMinTracker graph'
    runBacktrack mt (solve' graph')
  where
    solve' gr = step gr >>= \case
        Left done -> return done
        Right gr' -> solve' gr'

solveAll :: Graph s
      -> IO [Graph s]
solveAll graph' = do
    mt <- initMinTracker graph'
    runBacktrackAll mt (solve' graph')
  where
    solve' gr = step gr >>= \case
        Left done -> return done
        Right gr' -> solve' gr'


step :: Graph s
     -> Backtrack (Either (Graph s) (Graph s))
step graph' = MT.popMinNode >>= \case
    Nothing -> return $ Left graph'
    Just n  -> Right <$> collapse n graph'
{-# INLINE step #-}

collapse :: G.Vertex -> Graph s -> Backtrack (Graph s)
collapse v g = do
    getQuantum v >>= \case
      (Quantum (Observed{})) -> error "Can't collapse an already collapsed node!"
      before@(Quantum (Unknown xs :: SuperPos f)) -> do
        let options = otoList xs
        choice <- select options <|> (setQuantum v before *> backtrack)
        setQuantum v (Quantum (Observed choice :: SuperPos f))
        propagate (v, toDyn choice) g
{-# INLINE collapse #-}


debugStepper :: (Graph s -> IO ())
             -> Graph s
             -> IO (Graph s)
debugStepper stepHandler gr = do
    mt <- initMinTracker gr
    runBacktrack mt (debugStepper' gr)
  where
    debugStepper' gr' = do
      liftIO $ stepHandler gr'
      step gr' >>= \case
          Left done -> return done
          Right gr'' -> debugStepper' gr''

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


propagate :: forall s. (Vertex, DChoice) -> Graph s -> Backtrack (Graph s)
propagate (from', choice) g = do
    allEdges <- edgesFrom from'
    foldM step' g allEdges
    where
      step' :: Graph s -> (Vertex, DFilter) -> Backtrack (Graph s)
      step' g' e = propagateSingle choice e g'
{-# INLINE propagate #-}

propagateSingle :: DChoice -> (Vertex, DFilter) -> Graph s -> Backtrack (Graph s)
propagateSingle choice (to', dfilter) g = do
    modifyQuantum to' alterQ >>= \case
      Nothing -> return g
      Just newEntropy -> MT.setNodeEntropy to' newEntropy *> return g
  where
    alterQ :: Quantum -> (Quantum, Maybe Int)
    alterQ (Quantum (Unknown xs :: SuperPos f)) = do
        let filteredDown = (forceDyn $ dynApp (dynApp dfilter choice) (toDyn xs) :: f)
         in (Quantum (Unknown filteredDown), Just $ olength filteredDown)
    alterQ q = (q, Nothing)
{-# INLINE propagateSingle #-}
