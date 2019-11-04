{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Grid where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Functor.Rep as R
import Data.Distributive
import Control.Lens hiding (Context)
import qualified Data.Set.NonEmpty as NE
import qualified Graph as G
import WFC
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import GHC.Generics (Generic)
import Control.Arrow ((&&&))

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
    deriving (Eq, Show, Ord, Generic)
    deriving anyclass Hashable

data Grid a =
    Grid { _graph   :: G.Graph Coord Dir a
         , _rows    :: Row
         , _cols    :: Col
         }
    deriving (Show)

makeLenses ''Grid

instance Functor Grid where
  fmap f = graph . mapped %~ f


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
    grid = mkDiagGraph numRows numCols
    (numRows, numCols, rows) = rectangularize txt
    labeledGrid :: Grid Char
    labeledGrid = (\c -> (M.findWithDefault ' ' c nodeChars)) <$> grid


rectangularize :: T.Text -> (Row, Col, [T.Text])
rectangularize txt = (length lines', maxLength, padded)
  where
    lines' = T.lines txt
    maxLength = maximum . fmap T.length $ lines'
    padded = T.justifyLeft maxLength ' ' <$> lines'

mkDiagGraph :: Row -> Col -> Grid Coord
mkDiagGraph rows cols =
    Grid { _graph   = G.newGraph vertices' edges'
         , _rows    = rows
         , _cols    = cols
         }
  where
    vertices' :: [(Coord, Coord)]
    vertices' = fmap (id &&& id) slots
    slots :: [Coord]
    slots = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
    edges' :: [(Coord, Coord, Dir)]
    edges' = do
        (r, c) <- slots
        (to', e) <- [ ((r - 1, c - 1), NW)
                    , ((r - 1, c    ), N)
                    , ((r - 1, c + 1), NE)
                    , ((r    , c + 1), E)
                    , ((r + 1, c + 1), SE)
                    , ((r + 1, c    ), S)
                    , ((r + 1, c - 1), SW)
                    , ((r    , c - 1), W)
                    ]
        return ((r, c), to', e)

-- mkDiagGraph :: Row -> Col -> Grid Coord
-- mkDiagGraph rows cols =
--     Grid { _graph   = gr
--          , _rows    = rows
--          , _cols    = cols
--          }
--   where
--     (_, (nodemap, gr)) = NM.run G.empty $ do
--            let slots = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
--            for_ slots insMapNodeM
--            for_ slots $ \(r, c) -> do
--                let addE = addEdge (r, c)
--                addE NW (r - 1, c - 1)
--                addE N  (r - 1, c    )
--                addE NE (r - 1, c + 1)
--                addE E  (r    , c + 1)
--                addE SE (r + 1, c + 1)
--                addE S  (r + 1, c    )
--                addE SW (r + 1, c - 1)
--                addE W  (r    , c - 1)

-- mkOrthoGraph :: Row -> Col -> Grid Coord
-- mkOrthoGraph rows cols =
--     Grid { _graph   = gr
--          , _nodeMap = nodemap
--          , _rows    = rows
--          , _cols    = cols
--          }
--   where
--     (_, (nodemap, gr)) = NM.run G.empty $ do
--            let slots = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
--            for_ slots insMapNodeM
--            for_ slots $ \(r, c) -> do
--                let addE = addEdge (r, c)
--                addE N  (r - 1, c    )
--                addE E  (r    , c + 1)
--                addE S  (r + 1, c    )
--                addE W  (r    , c - 1)


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
        return $ grid ^?! graph . G.valueAtKey (r, c)

printOption :: Option Char ->  T.Text
printOption o = T.pack $ unlines (fmap (R.index o) <$>
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
  deriving (Show, Foldable)

superPosFilter :: (s -> Bool) -> SuperPos s -> WFC (SuperPos s)
superPosFilter _ o@(Observed{}) = return o
superPosFilter p (Unknown s) =
    maybe backtrack (pure . Unknown) . NE.nonEmptySet $ NE.filter p s

makePrisms ''SuperPos

fromObserved :: SuperPos a -> a
fromObserved (Observed a) = a
fromObserved (Unknown _) = error "fromObserved Error!"

collapseOption :: Option a -> a
collapseOption = flip R.index C

addMirrored :: SuperPos Position -> SuperPos Position
addMirrored (Unknown xs) = Unknown . NE.union xs . NE.map flipper $ xs
  where
    flipper (Option nw n ne
                    w  c e
                    sw s se)
      =  Option ne n nw
                e  c  w
                se s sw

addMirrored x = x

collectSuperPositions :: Grid Char -> Maybe (SuperPos Position)
collectSuperPositions grid
  = Unknown <$> NE.nonEmptySet allEdges
  where
    allEdges :: S.Set Position
    allEdges =
        grid ^. graph . G.edges . itraversed . withIndex . folding (uncurry buildOption) . to S.singleton
    findEdge :: G.Vertex -> HM.HashMap Dir G.Vertex -> Dir -> Maybe G.Vertex
    findEdge n _ C = Just n
    findEdge _ m d = m ^? ix d
    buildOption :: G.Vertex -> HM.HashMap Dir G.Vertex -> Maybe Position
    buildOption n m = do
        opts <- sequenceA $ tabulate (findEdge n m)
        traverse (\n -> grid ^? graph . G.valueAt n ) ( opts)
