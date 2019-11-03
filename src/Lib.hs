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
import WFC
import Control.Monad.IO.Class
import qualified Data.Set.NonEmpty as NE
import Text.Printf
import Data.Functor.Compose
import qualified MinTracker as MT
import Data.Maybe

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

-- minWavinessNode :: Grid (SuperPos p) -> WFC (Maybe Node)
-- minWavinessNode grid = do
--     MT.getMinNode
  -- where
  --   minNode = fst <$> minimumByOf (graph . to labNodes . folded . filtered (has $ _2 . _Unknown)) (compare `on` entropyOf . snd) grid

entropyOf :: (SuperPos p) -> Maybe Int
entropyOf (Unknown s) = Just $ NE.size s
entropyOf (Observed _) = Nothing

solve :: Eq p => Grid (SuperPos (Option p)) -> WFC (Grid (Option p))
solve grid = step grid >>= \case
    Left done -> return done
    Right grid' -> solve grid'

step :: Eq p => Grid (SuperPos (Option p)) -> WFC (Either (Grid (Option p)) (Grid (SuperPos (Option p))))
step grid = MT.popMinNode >>= \case
    Nothing -> return $ Left (fromObserved <$> grid)
    Just n -> do
        grid' <- collapse gridFilter n grid
        return $ Right grid'


collapse :: forall p. (p -> Dir -> (p -> Bool)) -> Node -> Grid (SuperPos p) -> WFC (Grid (SuperPos p))
collapse nFilter n grid = do
    -- choice <- rselectWithTrigger (const $ putStrLn "backtrack") $ grid ^.. graph . ctxAt n . ctxLabel . folded
    choice <- rselect $ grid ^.. graph . to (lab ?? n)  . _Just . folded
    let picked = grid & graph . ctxAt n . ctxLabel .~ Observed choice
    result <- foldM (propagate choice) picked propTargets
    return result
  where
    propagate :: p -> Grid (SuperPos p) -> (Dir, Node) -> WFC (Grid (SuperPos p))
    propagate choice gr (d, n) =
        gr & graph . ctxAt n . ctxLabel . filtered (has _Unknown) %%~ prop n choice d
    prop n choice d s = do
        new <- superPosFilter (nFilter choice d) s
        case entropyOf new of
          Nothing -> return new
          Just ent -> do
              MT.setNodeEntropy n ent
              return new

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

type Debug = Bool

initMinTracker :: forall p. Grid (SuperPos p) -> MT.MinTracker
initMinTracker grid = MT.fromList (allEntropies ^.. traversed . below _Just)
    where
      allEntropies = allNodes & traversed . _2 %~ entropyOf
      allNodes :: [LNode (SuperPos p)]
      allNodes =  grid ^.. graph . folding labNodes

run :: Maybe (Grid (SuperPos (Option Char)) -> IO ()) -> Int -> Int -> T.Text -> IO ()
run debugHandle rows cols txt = do
    let srcGrid = gridFromText txt
    let positions = collectSuperPositions srcGrid
    liftIO $ printf "num positions: %d\n" (length . Compose $ positions)
    -- liftIO . (traverse_ . traverse_) (T.putStrLn . printOption) $ positions
    pos <- maybe (fail "No possible states!") return positions
    let startGrid = generateGrid pos rows cols
    let minTracker = initMinTracker startGrid
    runWFC minTracker $ do
        debugStepper debugHandle startGrid
        return ()
        -- liftIO . T.putStrLn . gridToText $ flatten result

    -- traverse_ (liftIO . putStrLn . printOption) positions
    -- result <- solve startGrid
    -- liftIO . T.putStrLn . gridToText $ flatten result
    return ()
