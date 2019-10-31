{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib where

import qualified Data.Text as T
import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import Text.RawString.QQ (r)
import Control.Monad
import qualified Data.Map as M

type Row = Int
type Col = Int
type Coord = (Row, Col)

data Dir = N | E | S | W
  deriving (Eq, Show, Ord)

type N loc val = (loc, val)

graphFromText :: T.Text -> Gr (N Coord Char) Dir
graphFromText txt = mkGraph nodes edges
  where
    nodes :: [LNode (N Coord Char)]
    nodes = zip [0..] (labelChars rows)
    coordMap :: M.Map Coord Node
    coordMap = M.fromList (fmap toMapping nodes)
    toMapping :: (LNode (N Coord Char))-> (Coord, Node)
    toMapping (node, (coord, _)) = (coord, node)
    edges :: [LEdge Dir]
    edges = gridEdges coordMap numRows numCols
    (numRows, numCols, rows) = rectangularize txt

gridEdges :: M.Map Coord Node -> Row -> Col -> [LEdge Dir]
gridEdges coordMap rows cols = do
    r <- [0..rows - 1]
    c <- [0..cols - 1]
    Just node <- pure $ M.lookup (r, c) coordMap
    (r', c', dir) <- [(r, c + 1, E), (r + 1, c, S)]
    Just otherNode <- pure $ M.lookup (r', c') coordMap
    return (node, otherNode, dir)

labelChars :: [T.Text] -> [(Coord, Char)]
labelChars rows = concat labeled
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
