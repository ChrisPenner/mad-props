{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Grid where

import qualified Data.Text as T
import Data.Graph.Inductive hiding ((&))
import qualified Data.Graph.Inductive.NodeMap as NM
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable
import Data.Functor.Rep as R
import Data.Distributive
import qualified Data.List as L
import Data.Maybe
import Control.Lens hiding (Context)
import qualified Data.Set.NonEmpty as NE


type Coord = (Row, Col)
type Row = Int
type Col = Int

data Dir
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW
    | C
    deriving (Eq, Show, Ord)

data Grid a =
    Grid { _graph   :: Gr a Dir
         , _nodeMap :: (NodeMap Coord)
         , _rows    :: Row
         , _cols    :: Col
         }
    deriving (Show)

makeLenses ''Grid

instance Functor Grid where
  fmap f = graph %~ nmap f


instance Distributive Option where
  distribute = distributeRep

instance Representable Option where
  type Rep Option = Dir
  index (Option nw n ne w c e sw s se) d =
      case d of
          NW -> nw
          N -> n
          NE -> ne
          E -> e
          SE -> se
          S -> s
          SW -> sw
          W -> w
          C -> c
  tabulate f =
      Option
       (f NW) (f N) (f  NE)
       (f W ) (f C) (f  E )
       (f SW) (f S) (f  SE)

data Option a =
    Option
      a a a
      a a a
      a a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


gridFromText :: T.Text -> Grid Char
gridFromText txt = labeledGrid
  where
    nodeChars :: M.Map Coord Char
    nodeChars = (labelChars rows)
    grid = mkGridGraph numRows numCols
    (numRows, numCols, rows) = rectangularize txt
    labeledGrid :: Grid Char
    labeledGrid = (\c -> (M.findWithDefault ' ' c nodeChars)) <$> grid


rectangularize :: T.Text -> (Row, Col, [T.Text])
rectangularize txt = (length lines', maxLength, padded)
  where
    lines' = T.lines txt
    maxLength = maximum . fmap T.length $ lines'
    padded = T.justifyLeft maxLength ' ' <$> lines'

mkGridGraph :: Row -> Col -> Grid Coord
mkGridGraph rows cols =
    Grid { _graph   = gr
         , _nodeMap = nodemap
         , _rows    = rows
         , _cols    = cols
         }
  where
    (_, (nodemap, gr)) = NM.run empty $ do
           let slots = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
           for_ slots insMapNodeM
           for_ slots $ \(r, c) -> do
               let addE = addEdge (r, c)
               addE NW (r - 1, c - 1)
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


gridToText :: Grid Char -> T.Text
gridToText grid = T.pack . unlines $ do
    r <- [0..(grid ^. rows) - 1]
    return $ do
        c <- [0..(grid ^. cols) - 1]
        return . fromJust $ lab (grid ^. graph) (fst $ mkNode_ (grid ^. nodeMap) (r, c))


printOption :: Option Char -> String
printOption o = unlines (fmap (R.index o) <$>
    [ [NW, N, NE]
    , [W,  C, E ]
    , [SW, S, SE]])

gridFilter :: (Eq a) => Option a -> Dir -> (Option a -> Bool)
gridFilter choice dir other =
    R.index choice dir
      == R.index other C
    && R.index  other (flipDir dir)
      == R.index choice C

flipDir :: Dir -> Dir
flipDir NW = SE
flipDir N = S
flipDir NE = SW
flipDir E = W
flipDir SE = NW
flipDir S = N
flipDir SW = NE
flipDir W = E
flipDir C = C

type Position = Option Char
data SuperPos a =
    Observed a | Unknown (NE.NESet a)

collapseOption :: Option a -> a
collapseOption = flip R.index C

collectSuperPositions :: Grid Char -> Maybe (SuperPos Position)
collectSuperPositions grid
  = fmap Unknown . NE.nonEmptySet . S.fromList . catMaybes $ toPosition <$> allNodes
    where
      gr = grid ^. graph
      allNodes = nodes gr
      toPosition :: Node -> Maybe Position
      toPosition n =
          let linked = context gr n
           in sequenceA $ tabulate (findEdge linked)
      findEdge :: Context Char Dir -> Dir -> Maybe Char
      findEdge ctx C = Just $ lab' ctx
      findEdge ctx dir = do
          (n, _) <- L.find ((== dir) . snd) $ lsuc' ctx
          lab gr n
