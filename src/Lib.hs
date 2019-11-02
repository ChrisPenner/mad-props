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
import Data.Functor
import Data.Monoid
import Control.Applicative
import GHC.Stack
import Backtrack
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE

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

treeText :: T.Text
treeText = T.filter (/= ' ')
  [R.r|.!.|.!.#.#
       -------#-#
       .|..!.|#.#
       ---------#|]


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

solve :: Eq p => Grid (SuperPos (Option p)) -> Backtrack (Grid (Option p))
solve grid = step grid >>= \case
    Nothing -> return (pickFirst <$> grid)
    Just grid' -> solve grid'

step :: Eq p => Grid (SuperPos (Option p)) -> Backtrack (Maybe (Grid (SuperPos (Option p))))
step grid = case minWavinessNode grid of
    Nothing -> return Nothing
    Just n -> Just <$> collapse gridFilter n grid


pickFirst :: HasCallStack =>  S.Set a -> a
pickFirst = S.elemAt 0

collapse :: forall p e. (p -> Dir -> (p -> Bool)) -> Node -> Grid (SuperPos p) -> Backtrack (Grid (SuperPos p))
collapse nFilter n grid = do
    choice <- rselectWithTrigger (const $ print "backtrack") $ grid ^.. graph . ctxAt n . ctxLabel . folded
    lift $ guard (allOf (graph . allContexts . ctxLabel . to S.size) (>0) grid)
    return $ newGraph choice
  where
    newGraph choice = grid
        & graph . ctxAt n . ctxLabel .~ S.singleton choice
        &~ for propTargets
             (\(e, n') -> graph . ctxAt n' . ctxLabel . filtered ((>1) . S.size) %= S.filter (nFilter choice e))
    propTargets :: [(Dir, Node)]
    propTargets = grid ^.. graph . ctxAt n . ctxSuc . traversed

simplePos = collectSuperPositions $ gridFromText simpleText

forceSolve :: HasCallStack => Grid (SuperPos (Option Char)) -> Grid Char
forceSolve = fmap force
  where
    force s | S.null s = 'X'
    force s = flip R.index C . head . S.toList $ s

flatten :: HasCallStack => Grid (Option Char) -> Grid Char
flatten = fmap (flip R.index C)

laminate :: [T.Text] -> T.Text
laminate txts = T.unlines pieces
  where
    pieces =
        getZipList . getAp $ foldMap (Ap . ZipList . fmap (<> " ") . T.lines) txts

debugStepper :: Grid (SuperPos (Option Char)) -> Backtrack ()
debugStepper gr = do
    let currentStep = gridToText $ forceSolve gr
    let waviness = gridToText $ fmap showSuperPosSize gr
    liftIO . T.putStrLn $ laminate [currentStep, waviness]
    step gr >>= \case
        Nothing -> return ()
        Just gr' -> do
            -- print $ (lab (gr' ^. graph) <$> minWavinessNode gr')
            debugStepper gr'

showSuperPosSize :: SuperPos a -> Char
showSuperPosSize (Observed o) = '#'
showSuperPosSize (Unknown s)
  | NE.size s > 9 = '*'
  | otherwise = head $ show (NE.size s)

showSuperPos :: SuperPos (Option Char) -> Char
showSuperPos (Observed o) = collapseOption o
showSuperPos (Unknown s) = collapseOption $ NE.findMin s

test :: IO ()
test = runBacktrack $ do
    let srcGrid = gridFromText simpleText
    let positions = collectSuperPositions srcGrid
    case positions of
        Nothing -> liftIO $ putStrLn "No possible states!"
        Just pos -> do
            let startGrid = generateGrid pos 5 5
            debugStepper startGrid
    -- traverse_ (liftIO . putStrLn . printOption) positions
    -- result <- solve startGrid
    -- liftIO . T.putStrLn . gridToText $ flatten result
    return ()
