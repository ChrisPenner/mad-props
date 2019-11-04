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
import Grid
import Control.Monad
import Data.Functor
import Data.Monoid
import Control.Applicative
import GHC.Stack
import WFC
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE
import Text.Printf
import Data.Functor.Compose
import qualified MinTracker as MT

generateGrid :: p -> Row -> Col -> Grid p
generateGrid positions rows cols = mkDiagGraph rows cols $> positions

entropyOf :: (SuperPos p) -> Maybe Int
entropyOf (Unknown s) = Just $ NE.size s
entropyOf (Observed _) = Nothing

solve :: Eq p => Grid (SuperPos (Option p)) -> WFC (Grid p)
solve grid = step grid >>= \case
    Left done -> return $ flatten done
    Right grid' -> solve grid'

step :: Eq p => Grid (SuperPos (Option p)) -> WFC (Either (Grid (Option p)) (Grid (SuperPos (Option p))))
step grid = MT.popMinNode >>= \case
    Nothing -> return $ Left (fromObserved <$> grid)
    Just n -> do
        grid' <- collapse gridFilter n grid
        return $ Right grid'

collapse :: forall p. (p -> Dir -> (p -> Bool)) -> G.Vertex -> Grid (SuperPos p) -> WFC (Grid (SuperPos p))
collapse nFilter n grid = do
    -- choice <- rselectWithTrigger (const $ putStrLn "backtrack") $ grid ^.. graph . ctxAt n . ctxLabel . folded
    choice <- rselect $ grid ^.. graph . G.valueAt n . folded
    let picked = grid & graph . G.valueAt n .~ Observed choice
    result <- foldM (propagate choice) picked propTargets
    return result
  where
    propagate :: p -> Grid (SuperPos p) -> (Dir, G.Vertex) -> WFC (Grid (SuperPos p))
    propagate choice gr (d, n) =
        gr & graph . G.valueAt n . filtered (has _Unknown) %%~ prop n choice d
    prop n choice d s = do
        new <- superPosFilter (nFilter choice d) s
        case entropyOf new of
          Nothing -> return new
          Just ent -> do
              MT.setNodeEntropy n ent
              return new

    propTargets :: [(Dir, G.Vertex)]
    propTargets = grid ^.. graph . G.edgesFrom n

showSuper :: HasCallStack => Grid (SuperPos (Option Char)) -> Grid Char
showSuper = fmap force
  where
    force s | null s = 'X'
    force (Unknown x) | length x == 1 = collapseOption $ NE.findMin x
                      | otherwise = ' '
    force (Observed c) = collapseOption c

flatten :: HasCallStack => Grid (Option p) -> Grid p
flatten = fmap collapseOption

laminate :: [T.Text] -> T.Text
laminate txts = T.unlines pieces
  where
    pieces =
        getZipList . getAp $ foldMap (Ap . ZipList . fmap (<> " ") . T.lines) txts

debugStepper :: Maybe (Grid (SuperPos (Option Char)) -> IO ()) -> Grid (SuperPos (Option Char)) -> WFC (Grid (Option Char))
debugStepper stepHandler gr = do
    liftIO $ maybe (return ()) ($ gr) stepHandler
    step gr >>= \case
        Left done -> return done
        Right gr' -> do
            -- print $ (lab (gr' ^. graph) <$> minWavinessNode gr')
            debugStepper stepHandler gr'

showSuperPosSize :: SuperPos a -> Char
showSuperPosSize (Observed _) = '#'
showSuperPosSize (Unknown s)
  | NE.size s > 9 = '*'
  | otherwise = head $ show (NE.size s)

showSuperPos :: SuperPos (Option Char) -> Char
showSuperPos (Observed o) = collapseOption o
showSuperPos (Unknown s) = collapseOption $ NE.findMin s

initMinTracker :: forall p. Grid (SuperPos p) -> MT.MinTracker
initMinTracker grid = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies = allNodes & traversed . _2 %~ entropyOf
      allNodes :: [(G.Vertex, SuperPos p)]
      allNodes =  grid ^@.. graph . itraversed

run :: Maybe (Grid (SuperPos (Option Char)) -> IO ()) -> Int -> Int -> T.Text -> IO ()
run debugHandle rows cols txt = do
    let srcGrid = gridFromText txt
    -- print srcGrid
    let positions = collectSuperPositions srcGrid
    liftIO $ printf "num positions: %d\n" (length . Compose $ positions)
    -- liftIO . (traverse_ . traverse_) (T.putStrLn . printOption) $ positions
    pos <- maybe (fail "No possible states!") return positions
    let startGrid = generateGrid pos rows cols
    let minTracker = initMinTracker startGrid
    runWFC minTracker $ do
        _result <- debugStepper debugHandle startGrid
        -- liftIO . T.putStrLn . gridToText . flatten $ _result
        return ()
    return ()
