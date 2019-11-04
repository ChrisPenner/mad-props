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

import qualified Graph as G
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens hiding (Context)
import Graph
import Grid
import Control.Monad
import Data.Monoid
import Data.Hashable
import Control.Applicative
import GHC.Stack
import WFC
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE
import Text.Printf
import Data.Functor.Compose
import qualified MinTracker as MT

type PropFilter f e a = f a -> e -> (f a -> Bool)

entropyOf :: (SuperPos p) -> Maybe Int
entropyOf (Unknown s) = Just $ NE.size s
entropyOf (Observed _) = Nothing

solve :: (Eq p, Eq e, Hashable e) => PropFilter f e p -> Graph k e (SuperPos (f p)) -> WFC (Graph k e (f p))
solve propFilter grid = step propFilter grid >>= \case
    Left done -> return done
    Right grid' -> solve propFilter grid'

step :: (Eq p, Eq e, Hashable e) => PropFilter f e p -> Graph k e (SuperPos (f p)) -> WFC (Either (Graph k e (f p)) (Graph k e (SuperPos (f p))))
step propFilter graph' = MT.popMinNode >>= \case
    Nothing -> return $ Left (fromObserved <$> graph')
    Just n -> do
        newGraph <- collapse propFilter n graph'
        return $ Right newGraph

collapse :: forall p k e. (Eq e, Hashable e) => (p -> e -> (p -> Bool)) -> G.Vertex -> Graph k e (SuperPos p) -> WFC (Graph k e (SuperPos p))
collapse nFilter n graph' = do
    -- choice <- rselectWithTrigger (const $ putStrLn "backtrack") $ grid ^.. graph . ctxAt n . ctxLabel . folded
    choice <- rselect $ graph' ^.. G.valueAt n . folded
    let picked = graph' & G.valueAt n .~ Observed choice
    result <- foldM (propagate choice) picked propTargets
    return result
  where
    propagate :: p -> Graph k e (SuperPos p) -> (e, G.Vertex) -> WFC (Graph k e (SuperPos p))
    propagate choice gr (d, n) =
        gr & G.valueAt n . filtered (has _Unknown) %%~ prop n choice d
    prop n choice d s = do
        new <- superPosFilter (nFilter choice d) s
        case entropyOf new of
          Nothing -> return new
          Just ent -> do
              MT.setNodeEntropy n ent
              return new

    propTargets :: [(e, G.Vertex)]
    propTargets = graph' ^.. G.edgesFrom n

showSuper :: HasCallStack => Graph k e (SuperPos (Option Char)) -> Graph k e Char
showSuper = fmap force
  where
    force s | null s = 'X'
    force (Unknown x) | length x == 1 = collapseOption $ NE.findMin x
                      | otherwise = ' '
    force (Observed c) = collapseOption c

flatten :: HasCallStack => Graph k e (Option p) -> Graph k e p
flatten = fmap collapseOption

laminate :: [T.Text] -> T.Text
laminate txts = T.unlines pieces
  where
    pieces =
        getZipList . getAp $ foldMap (Ap . ZipList . fmap (<> " ") . T.lines) txts

debugStepper :: (Eq p, Eq e, Hashable e) => Maybe (Graph k e (SuperPos (f p)) -> IO ()) -> PropFilter f e p -> Graph k e (SuperPos (f p)) -> WFC (Graph k e (f p))
debugStepper stepHandler propFilter gr = do
    liftIO $ maybe (return ()) ($ gr) stepHandler
    step propFilter gr >>= \case
        Left done -> return done
        Right gr' -> do
            -- print $ (lab (gr' ^. graph) <$> minWavinessNode gr')
            debugStepper stepHandler propFilter gr'

showSuperPosSize :: SuperPos a -> Char
showSuperPosSize (Observed _) = '#'
showSuperPosSize (Unknown s)
  | NE.size s > 9 = '*'
  | otherwise = head $ show (NE.size s)

showSuperPos :: SuperPos (Option Char) -> Char
showSuperPos (Observed o) = collapseOption o
showSuperPos (Unknown s) = collapseOption $ NE.findMin s

initMinTracker :: forall p k e. Graph k e (SuperPos p) -> MT.MinTracker
initMinTracker graph' = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies = allNodes & traversed . _2 %~ entropyOf
      allNodes :: [(G.Vertex, SuperPos p)]
      allNodes =  graph' ^@.. itraversed

run :: Maybe (Graph Coord Dir (SuperPos (Option Char)) -> IO ()) -> Int -> Int -> T.Text -> IO ()
run debugHandle rows cols txt = do
    let srcGrid = gridFromText txt
    -- print srcGrid
    let positions = collectSuperPositions srcGrid
    liftIO $ printf "num positions: %d\n" (length . Compose $ positions)
    -- liftIO . (traverse_ . traverse_) (T.putStrLn . printOption) $ positions
    pos <- maybe (fail "No possible states!") return positions
    let startGrid = generateGrid pos rows cols
    let minTracker = initMinTracker (startGrid ^. graph)
    runWFC minTracker $ do
        _result <- debugStepper debugHandle gridFilter (startGrid ^. graph)
        liftIO . T.putStrLn . gridToText . (\g -> Grid g rows cols) . flatten $ _result
        return ()
    return ()
