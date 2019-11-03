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
import GraphLens
import Control.Lens hiding (Context)
import Grid
import Control.Monad
import Data.Functor
import Data.Monoid
import Control.Applicative
import GHC.Stack
import Backtrack
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE
import Data.Function (on)
import Data.Maybe
import Text.Printf
import Data.Functor.Compose
import Data.Foldable

testText :: T.Text
testText = [R.r|one
a longer thing
there
|]

simpleText :: T.Text
simpleText = T.filter (/= ' ')
  [R.r|=%=%=%=%=%
       %=%=%=%=%=
       $*$$=*=$$$
       %=%=%=%=%=
       =%=%=%=%=%|]

treeText :: T.Text
treeText = T.filter (/= ' ')
  [R.r|..|..|
       +-++-+
       |..|.|
       +--|-+
       +--+.||]

exText :: T.Text
exText = T.filter (/= ' ')
  [R.r|x...x...x...
       .\./.\./.\./
       ..x...x...x.
       ./.\./.../.\
       x...x...x...
       .\./.\./.\./|]

vineText :: T.Text
vineText = T.unlines . drop 1 . T.lines $
  [R.r|
   __    __        __          __    __        __          __    __
  (//    \\)    __(//   __    (//    \\)    __(//   __    (//    \\)
  /"      / __  \\)"    \\)_  /"      / __  \\)"    \\)_  /"      / __
'|-..__..-''\_''-.\__..-''  '|-..__..-''\_''-.\__..-''  '|-..__..-''\
(\\  \_    _(\\      _/     (\\  \_    _(\\      _/     (\\  \_    //)
 ""  (\\  //)""     //)      ""  (\\  //)""     //)      ""  (\\   ""
      ""  ""        ""            ""  ""        ""            ""|]

tiledText :: T.Text
tiledText = T.unlines . drop 1 . T.lines $
  [R.r|
 _\__/__/__\__\__\__\__/__\__\__/__/__\__\_
 \  /  /  \  \  \  \  /  \  \  /  /  \  \
 /  \  \  /   ) /   )(    ) /  \  \  /   )
_\__/__/__\__/__\__/__\__/__\__/__/__\__/_
 \  /  /  \  /  \  /  \  /  \  /  /  \  /
 / (  (    ) \  /  \  / (   / (  (    ) \
_\__\__\__/__/__\__/__\__\__\__\__\__/__/_
 \  \  \  /  /  \  /  \  \  \  \  \  /  /
 /   ) /  \ (   / (   /   ) /   ) /  \ (
_\__/__\__/__\__\__\__\__/__\__/__\__/__\_
 \  /  \  /  \  \  \  \  /  \  /  \  /  \
 /  \   )(    )  )  )  )(   /  \   )(    )
_\__/__/__\__/__/__/__/__\__\__/__/__\__/_
 \  /  /  \  /  /  /  /  \  \  /  /  \  /
 /  \ (   / (  (  (  (   /  /  \ (   / (
_\__/__\__\__\__\__\__\__\__\__/__\__\__\_
 \  /  \  \  \  \  \  \  \  \  /  \  \  \
 / (    )  ) /   )  )  ) /  / (    )  ) /
_\__\__/__/__\__/__/__/__\__\__\__/__/__\_
 \  \  /  /  \  /  /  /  \  \  \  /  /  \
 /   )(  (    ) \ (  (   /  /   )(  (    )
_\__/__\__\__/__/__\__\__\__\__/__\__\__/_
 \  /  \  \  /  /  \  \  \  \  /  \  \  /
  )(    ) /  \  \  /   ) /   )(    ) /  \
_/__\__/__\__/__/__\__/__\__/__\__/__\__/_
 /  \  /  \  /  /  \  /  \  /  \  /  \  /|]


generateGrid :: p -> Row -> Col -> Grid p
generateGrid positions rows cols = mkDiagGraph rows cols $> positions

minWavinessNode :: Grid (SuperPos p) -> Maybe Node
minWavinessNode grid = min'
  where
    min' =
        node'
        <$> minimumByOf (graph . allContexts . filtered (isJust . entropyOf . lab'))
                        (compare `on` entropyOf . lab')
                        grid

entropyOf :: (SuperPos p) -> Maybe Int
entropyOf (Unknown s) = Just $ NE.size s
entropyOf (Observed _) = Nothing

solve :: Eq p => Grid (SuperPos (Option p)) -> Backtrack (Grid (Option p))
solve grid = step grid >>= \case
    Left done -> return done
    Right grid' -> solve grid'

step :: Eq p => Grid (SuperPos (Option p)) -> Backtrack (Either (Grid (Option p)) (Grid (SuperPos (Option p))))
step grid = case minWavinessNode grid of
    Nothing -> return $ Left (fromObserved <$> grid)
    Just n -> Right <$> collapse gridFilter n grid


collapse :: forall p. (p -> Dir -> (p -> Bool)) -> Node -> Grid (SuperPos p) -> Backtrack (Grid (SuperPos p))
collapse nFilter n grid = do
    -- choice <- rselectWithTrigger (const $ putStrLn "backtrack") $ grid ^.. graph . ctxAt n . ctxLabel . folded
    choice <- rselect $ grid ^.. graph . ctxAt n . ctxLabel . folded
    let picked = grid & graph . ctxAt n . ctxLabel .~ Observed choice
    result <- foldM (propagate choice) picked propTargets
    return result
  where
    propagate :: p -> Grid (SuperPos p) -> (Dir, Node) -> Backtrack (Grid (SuperPos p))
    propagate choice gr (d, n) =
        gr & graph . ctxAt n . ctxLabel . filtered (has _Unknown) %%~ superPosFilter (nFilter choice d)
    -- newGraph choice = grid
    --     & graph . ctxAt n . ctxLabel .~ Observed choice
    --     &~ for propTargets
    --          (\(e, n') -> graph . ctxAt n' . ctxLabel . filtered (has _Unknown) %= S.filter (nFilter choice e))
    propTargets :: [(Dir, Node)]
    propTargets = grid ^.. graph . ctxAt n . ctxSuc . traversed

simplePos :: Maybe (SuperPos Position)
simplePos = collectSuperPositions $ gridFromText simpleText

showSuper :: HasCallStack => Grid (SuperPos (Option Char)) -> Grid Char
showSuper = fmap force
  where
    force s | null s = 'X'
    force (Unknown x) | length x == 1 = collapseOption $ NE.findMin x
                      | otherwise = ' '
    force (Observed c) = collapseOption c

flatten :: HasCallStack => Grid (Option Char) -> Grid Char
flatten = fmap collapseOption

laminate :: [T.Text] -> T.Text
laminate txts = T.unlines pieces
  where
    pieces =
        getZipList . getAp $ foldMap (Ap . ZipList . fmap (<> " ") . T.lines) txts

debugStepper :: Grid (SuperPos (Option Char)) -> Backtrack (Grid (Option Char))
debugStepper gr = do
    let currentStep = gridToText $ showSuper gr
    let waviness = gridToText $ fmap showSuperPosSize gr
    liftIO . T.putStrLn $ laminate [currentStep, waviness]
    step gr >>= \case
        Left done -> return done
        Right gr' -> do
            -- print $ (lab (gr' ^. graph) <$> minWavinessNode gr')
            debugStepper gr'

showSuperPosSize :: SuperPos a -> Char
showSuperPosSize (Observed _) = '#'
showSuperPosSize (Unknown s)
  | NE.size s > 9 = '*'
  | otherwise = head $ show (NE.size s)

showSuperPos :: SuperPos (Option Char) -> Char
showSuperPos (Observed o) = collapseOption o
showSuperPos (Unknown s) = collapseOption $ NE.findMin s

test :: IO ()
test = runBacktrack $ do
    let srcGrid = gridFromText tiledText
    let positions = collectSuperPositions srcGrid
    liftIO $ printf "num positions: %d\n" (length . Compose $ positions)
    -- liftIO $ print positions
    -- liftIO . (traverse_ . traverse_) (T.putStrLn . printOption) $ positions
    case positions of
        Nothing -> liftIO $ putStrLn "No possible states!"
        Just pos -> do
            let startGrid = generateGrid pos 5  5
            -- result <- debugStepper startGrid
            result <- solve startGrid
            liftIO . T.putStrLn . gridToText $ flatten result

    -- traverse_ (liftIO . putStrLn . printOption) positions
    -- result <- solve startGrid
    -- liftIO . T.putStrLn . gridToText $ flatten result
    return ()
