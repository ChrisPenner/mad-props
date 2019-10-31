{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import qualified Data.Text as T
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.NodeMap as NM
import Text.RawString.QQ (r)
import Control.Monad
import qualified Data.Map as M
import Data.Traversable

type Row = Int
type Col = Int
type Coord = (Row, Col)

data Dir
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW
    deriving (Eq, Show, Ord)

type N loc val = (loc, val)

graphFromText :: T.Text -> (M.Map Coord Char, NodeMap Coord, Gr Coord Dir)
graphFromText txt = (labelChars rows, coordMap, gridGraph)
  where
    nodes :: M.Map Coord Char
    nodes = (labelChars rows)
    (coordMap, gridGraph) = mkGridGraph numRows numCols
    (numRows, numCols, rows) = rectangularize txt

mkGridGraph :: Row -> Col -> (NodeMap Coord, Gr Coord Dir)
mkGridGraph rows cols = snd . NM.run empty $ do
    let slots = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
    for slots insMapNodeM
    for slots $ \(r, c) -> do
        let addE = addEdge (r, c)
        addE N  (r - 1, c    )
        addE NE (r - 1, c + 1)
        addE E  (r    , c + 1)
        addE SE (r + 1, c + 1)
        addE S  (r + 1, c    )
        addE SW (r + 1, c - 1)
        addE W  (r    , c - 1)

addEdge :: Ord a => a -> e -> a -> NodeMapM a e Gr ()
addEdge current e other = do
    mkEdgeM (current, other, e) >>= \case
      Nothing -> return ()
      Just _ -> void $ insMapEdgeM (current, other, e)

labelChars :: [T.Text] -> M.Map Coord Char
labelChars rows = M.fromList $ concat labeled
    where
      lines' :: [String]
      lines' = T.unpack <$> rows
      labeled :: [[(Coord, Char)]]
      labeled = do
          (row, trow) <- zip [0..] (zip [0..] <$> lines')
          return $ reassoc row <$> trow
      reassoc row (col, c) = ((row, col), c)

rectangularize :: T.Text -> (Row, Col, [T.Text])
rectangularize txt = (length lines', maxLength, padded)
  where
    lines' = T.lines txt
    maxLength = maximum . fmap T.length $ lines'
    padded = T.justifyLeft maxLength ' ' <$> lines'

txt :: T.Text
txt = [r|one
a longer thing
there
|]

when_ :: Monad m => Bool -> m a -> m ()
when_ cond = when cond . void

