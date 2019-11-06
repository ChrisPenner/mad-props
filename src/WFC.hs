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
import Control.Monad
import Data.Hashable
import WFC.Backtracking
import WFC.Types
import WFC.Graph
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE
import qualified WFC.MinTracker as MT
import Data.Foldable as F

entropyOf :: DSuperPos -> Maybe Int
entropyOf (forceDyn -> (Unknown s)) = Just $ length s
entropyOf (forceDyn -> (Observed _)) = Nothing

solve :: (Eq p, Eq e, Hashable e)
      => PropFilter f e p
      -> G.Graph k
      -> IO (G.Graph k)
solve propFilter graph' = runWFC mt (solve' graph')
  where
    solve' gr = step propFilter gr >>= \case
        Left done -> return done
        Right gr' -> solve' gr'
    mt = initMinTracker graph'

step :: (Eq p, Eq e, Hashable e)
     => PropFilter f e p
     -> G.Graph k
     -> Backtrack (Either (G.Graph k) (G.Graph k))
step propFilter graph' = MT.popMinNode >>= \case
    Nothing -> return $ Left graph'
    Just n  -> do
        newGraph <- collapse propFilter n graph'
        return $ Right newGraph

collapse :: forall k.
            G.Vertex
         -> G.Graph k
         -> Backtrack (G.Graph k)
collapse n graph' = do
    choice <- choose
    let picked = graph' & G.valueAt n .~ choice
    let (edges' :: [(Vertex, DFilter)]) = picked ^.. edgesFrom n
    foldM (prop choice) picked edges'
    -- result <- foldM (propagate choice) picked propTargets
    return result
  where
    -- We need to unwrap and wrap all at once to keep type vars happy
    choose :: Backtrack DSuperPos
    choose = rselect $ graph' ^.. G.valueAt n . folding (\(SuperPos' x) -> (SuperPos' . Observed) <$> F.toList x)

    prop :: DSuperPos -> G.Graph k -> (Vertex, DFilter) -> Backtrack (G.Graph k)
    prop (SuperPos') g (v, f) = undefined
        -- g & valueAt v %~ f (Ob)
      return g


    propagate :: p
              -> G.Graph k
              -> G.Vertex
              -> Backtrack (G.Graph k)
    propagate choice gr (d, n) =
        gr & G.valueAt n %%~ prop n choice d

    -- prop :: G.Vertex -> p -> e -> SuperPos p -> Backtrack (SuperPos p)
    -- prop n choice d s = do
    --     new <- _superPosFilter (nFilter choice d) s
    --     case entropyOf new of
    --         Nothing  -> return new
    --         Just ent -> do
    --             MT.setNodeEntropy n ent
    --             return new

    propTargets :: [(e, G.Vertex)]
    propTargets = graph' ^.. G.edgesFrom n

debugStepper :: (Eq p, Eq e, Hashable e)
             => (G.Graph k -> IO ())
             -> PropFilter f e p
             -> G.Graph k
             -> IO (G.Graph k)
debugStepper stepHandler propFilter gr = runWFC mt (debugStepper' gr)
  where
    mt = initMinTracker gr
    debugStepper' gr' = do
      liftIO $ stepHandler gr'
      step propFilter gr' >>= \case
          Left done -> return done
          Right gr'' -> debugStepper' gr''

initMinTracker :: forall p k e. G.Graph k -> MT.MinTracker
initMinTracker graph' = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies :: [(G.Vertex, Maybe Int)]
      allEntropies = allNodes & traversed . _2 %~ entropyOf
      allNodes :: [(G.Vertex, G.SuperPos')]
      allNodes =  graph' ^@.. values
