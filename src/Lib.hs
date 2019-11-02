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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Graph.Inductive hiding ((&))
import qualified Text.RawString.QQ as R
import qualified Data.Set as S
import Data.Traversable
import Data.Functor.Rep as R
import GraphLens
import Control.Lens hiding (Context)
import Grid
import Control.Monad
import Data.Maybe
import Data.Functor
import Data.Monoid
import Control.Applicative
import qualified Data.List as L
import GHC.Stack
import Data.Foldable
import Backtrack

testText :: T.Text
testText = [R.r|one
a longer thing
there
|]

simpleText :: T.Text
simpleText = T.filter (/= ' ')
  [R.r|=%=%=%=%=%
       %=%=%=%=%=
       =%=%=%=%=%
       $$$$$$$$$$
       %=%=%=%=%=
       =%=%=%=%=%
       %=%=%=%=%=|]

generateGrid :: p -> Row -> Col -> Grid p
generateGrid positions rows cols = mkGridGraph rows cols $> positions

minWavinessNode :: Grid (SuperPos p) -> Maybe Node
minWavinessNode grid = fmap node' . ufold go Nothing $ grid ^. graph
  where
    go :: Context (SuperPos p) b -> Maybe (Context (SuperPos p) b) -> Maybe (Context (SuperPos p) b)
    go nextCtx c | entropyOf nextCtx <= 1 = c
    go ctx (Just otherCtx)  | entropyOf ctx < entropyOf otherCtx = Just ctx
                            | otherwise = Just otherCtx
    go ctx Nothing = Just ctx
    entropyOf :: Context (SuperPos p) b -> Int
    entropyOf ctx = S.size (lab' ctx)

solve :: Eq p => Grid (SuperPos (Option p)) -> Grid (Option p)
solve grid = case step grid of
    Nothing -> pickFirst <$> grid
    Just grid' -> solve grid'

step :: Eq p => Grid (SuperPos (Option p)) -> Maybe (Grid (SuperPos (Option p)))
step grid = case minWavinessNode grid of
    Nothing -> Nothing
    Just n -> Just $ collapse gridFilter n grid


pickFirst :: HasCallStack =>  S.Set a -> a
pickFirst = S.elemAt 0

collapse :: forall p e. (p -> Dir -> (p -> Bool)) -> Node -> Grid (SuperPos p) -> Grid (SuperPos p)
collapse nFilter n grid = newGraph
  where
    newGraph = grid
        & graph . ctxAt n . ctxLabel .~ S.singleton choice
        &~ for propTargets
             (\(e, n') -> graph . ctxAt n' . ctxLabel . filtered ((>1) . S.size) %= S.filter (propFilter e))
    propTargets :: [(Dir, Node)]
    propTargets = grid ^.. graph . ctxAt n . ctxSuc . traversed
    choice :: p
    choice = grid ^?! graph . ctxAt n . ctxLabel . folded
    propFilter = nFilter choice

simplePos = collectSuperPositions $ gridFromText simpleText

forceSolve :: HasCallStack => Grid (SuperPos (Option Char)) -> Grid Char
forceSolve = fmap force
  where
    force s | S.null s = 'X'
    force s = flip R.index C . head . S.toList $ s

laminate :: [T.Text] -> T.Text
laminate txts = T.unlines pieces
  where
    pieces =
        getZipList . getAp $ foldMap (Ap . ZipList . fmap (<> " ") . T.lines) txts

debugStepper :: Grid (SuperPos (Option Char)) -> IO ()
debugStepper gr = do
    let currentStep = gridToText $ forceSolve gr
    let waviness = gridToText $ fmap szToChar gr
    T.putStrLn $ laminate [currentStep, waviness]
    case step gr of
        Nothing -> return ()
        Just gr' -> do
            -- print $ (lab (gr' ^. graph) <$> minWavinessNode gr')
            debugStepper gr'
  where
    szToChar :: S.Set a -> Char
    szToChar s | S.size s > 9 = '*'
               | otherwise = head $ show (S.size s)

test :: IO ()
test = do
    let srcGrid = gridFromText simpleText
    let positions = collectSuperPositions srcGrid
    -- T.putStrLn $ graphToText rows cols srcNM srcGr
    let startGrid = generateGrid positions 5 5
    -- print (startGrid & step)
    -- T.putStrLn $ gridToText . forceSolve . fromJust $ (startGrid & step)
    traverse_ (putStrLn . printOption) positions
    debugStepper startGrid
    T.putStrLn . gridToText . fmap (flip R.index C) $ solve startGrid
    -- let solved = flip R.index C <$> solve startGrid
    -- T.putStrLn $ gridToText solved

    return ()
