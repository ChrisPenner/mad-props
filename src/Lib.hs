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

testText :: T.Text
testText = [R.r|one
a longer thing
there
|]

simpleText :: T.Text
simpleText = T.filter (/= ' ')
  [R.r|..........
       ..........
       ..........
       ...=+=+...
       ...+=+=...
       ...=+=+...
       ..........
       ..........
       ..........|]

generateGrid :: p -> Row -> Col -> (NodeMap Coord, Gr p Dir)
generateGrid positions row col = (nodemap, nmap (const positions) gr)
    where
      (nodemap, gr) = mkGridGraph row col

minWavinessNode :: Gr (SuperPos p) e -> Maybe Node
minWavinessNode = fmap node' . ufold go Nothing
  where
    go :: Context (SuperPos p) b -> Maybe (Context (SuperPos p) b) -> Maybe (Context (SuperPos p) b)
    go ctx c | entropyOf ctx <= 1 = c
    go ctx (Just otherCtx)  | entropyOf ctx < entropyOf otherCtx = Just ctx
                            | otherwise = Just otherCtx
    go ctx Nothing = Just ctx
    entropyOf :: Context (SuperPos p) b -> Int
    entropyOf ctx = S.size (lab' ctx)

solve :: Eq p => Gr (SuperPos (Option p)) Dir -> Gr (Option p) Dir
solve gr = case step gr of
    Nothing -> nmap pickFirst $ gr
    Just gr' -> solve gr'

step :: Eq p => Gr (SuperPos (Option p)) Dir -> Maybe (Gr (SuperPos (Option p)) Dir)
step gr = case minWavinessNode gr of
    Nothing -> Nothing
    Just n -> Just $ collapse gridFilter n gr

pickFirst :: S.Set a -> a
pickFirst = S.elemAt 0

collapse :: forall p e. (p -> e -> (p -> Bool)) -> Node -> Gr (SuperPos p) e -> Gr (SuperPos p) e
collapse nFilter n gr = newGraph
  where
    newGraph = gr
        & ctxAt n . ctxLabel .~ S.singleton choice
        &~ for propTargets
             (\(e, n) -> ctxAt n . ctxLabel . filtered ((>1) . S.size) %= S.filter (propFilter e))
    propTargets :: [(e, Node)]
    propTargets = gr ^.. ctxAt n . ctxSuc . traversed
    choice :: p
    choice = gr ^?! ctxAt n . ctxLabel . folded
    propFilter = nFilter choice

simplePos =
    let (rows, cols, srcNM, srcGr) = graphFromText $ simpleText
     in collectSuperPositions srcGr

forceSolve :: Gr (SuperPos (Option Char)) Dir -> Gr Char Dir
forceSolve = nmap (flip R.index C . S.elemAt 0)

-- debugStepper :: Gr (SuperPos (Option Char)) Dir -> IO ()
-- debugStepper gr = 
--     forceSolve gr


test :: IO ()
test = do
    let (rows, cols, srcNM, srcGr) = graphFromText $ simpleText
    let positions = collectSuperPositions srcGr
    -- T.putStrLn $ graphToText rows cols srcNM srcGr
    let (destNM, startGrid) = generateGrid positions 3 3
    -- print (startGrid & step)
    T.putStrLn $ graphToText 3 3 destNM . forceSolve . fromJust $ (startGrid & step)
    let solved = nmap (flip R.index C) $ solve startGrid
    T.putStrLn $ graphToText 3 3 destNM solved

    return ()
