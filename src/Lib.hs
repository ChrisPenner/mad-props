{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Lib where

import qualified WFC.Graph as G
import qualified Data.Text as T
import Control.Lens hiding (Context)
import Control.Monad
import Data.Monoid
import Data.Hashable
import Control.Applicative
import WFC
import WFC.Types
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE
import qualified MinTracker as MT

entropyOf :: (SuperPos p) -> Maybe Int
entropyOf (Unknown s) = Just $ NE.size s
entropyOf (Observed _) = Nothing

solve :: (Eq p, Eq e, Hashable e) => PropFilter f e p -> G.Graph k e (SuperPos (f p)) -> WFC (G.Graph k e (f p))
solve propFilter grid = runWFC solve'
  where
    solve' = step propFilter grid >>= \case
        Left done -> return done
        Right grid' -> solve propFilter grid'

step :: (Eq p, Eq e, Hashable e)
     => PropFilter f e p
     -> G.Graph k e (SuperPos (f p))
     -> WFC (Either (G.Graph k e (f p)) (G.Graph k e (SuperPos (f p))))
step propFilter graph' = MT.popMinNode >>= \case
    Nothing -> return $ Left (fromObserved <$> graph')
    Just n  -> do
        newGraph <- collapse propFilter n graph'
        return $ Right newGraph

collapse :: forall p k e.
         (Eq e, Hashable e)
         => (p -> e -> (p -> Bool))
         -> G.Vertex
         -> G.Graph k e (SuperPos p)
         -> WFC (G.Graph k e (SuperPos p))
collapse nFilter n graph' = do
    choice <- rselect $ graph' ^.. G.valueAt n . folded
    let picked = graph' & G.valueAt n .~ Observed choice
    result <- foldM (propagate choice) picked propTargets
    return result
  where
    propagate :: p
              -> G.Graph k e (SuperPos p)
              -> (e, G.Vertex)
              -> WFC (G.Graph k e (SuperPos p))
    propagate choice gr (d, n) =
        gr & G.valueAt n . filtered (has _Unknown) %%~ prop n choice d

    prop :: G.Vertex -> p -> e -> SuperPos p -> WFC (SuperPos p)
    prop n choice d s = do
        new <- superPosFilter (nFilter choice d) s
        case entropyOf new of
            Nothing  -> return new
            Just ent -> do
                MT.setNodeEntropy n ent
                return new

    propTargets :: [(e, G.Vertex)]
    propTargets = graph' ^.. G.edgesFrom n

debugStepper :: (Eq p, Eq e, Hashable e)
             => (G.Graph k e (SuperPos (f p)) -> IO ())
             -> PropFilter f e p
             -> G.Graph k e (SuperPos (f p))
             -> WFC (G.Graph k e (f p))
debugStepper stepHandler propFilter gr = do
    liftIO $ stepHandler gr
    step propFilter gr >>= \case
        Left done -> return done
        Right gr' -> debugStepper stepHandler propFilter gr'

initMinTracker :: forall p k e. G.Graph k e (SuperPos p) -> MT.MinTracker
initMinTracker graph' = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies = allNodes & traversed . _2 %~ entropyOf
      allNodes :: [(G.Vertex, SuperPos p)]
      allNodes =  graph' ^@.. itraversed
