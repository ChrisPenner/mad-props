{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Grid where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Graph.Inductive hiding ((&))
import qualified Data.Graph.Inductive.NodeMap as NM
import qualified Text.RawString.QQ as R
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable
import Data.Foldable
import Data.Functor.Rep as R
import Data.Distributive
import qualified Data.List as L
import Data.Maybe
import GraphLens
import Control.Lens hiding (Context)

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


graphFromText :: T.Text -> (Row, Col, NodeMap Coord, Gr Char Dir)
graphFromText txt = (numRows, numCols, coordMap, labeledGraph)
  where
    nodeChars :: M.Map Coord Char
    nodeChars = (labelChars rows)
    (coordMap, gridGraph) = mkGridGraph numRows numCols
    (numRows, numCols, rows) = rectangularize txt
    labeledGraph :: Gr Char Dir
    labeledGraph
      = nmap (\c -> (M.findWithDefault ' ' c nodeChars)) gridGraph


rectangularize :: T.Text -> (Row, Col, [T.Text])
rectangularize txt = (length lines', maxLength, padded)
  where
    lines' = T.lines txt
    maxLength = maximum . fmap T.length $ lines'
    padded = T.justifyLeft maxLength ' ' <$> lines'

mkGridGraph :: Row -> Col -> (NodeMap Coord, Gr Coord Dir)
mkGridGraph rows cols = snd . NM.run empty $ do
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


graphToText :: Row -> Col -> NodeMap Coord -> Gr Char e -> T.Text
graphToText rows cols nm gr = T.pack . unlines $ do
    r <- [0..rows - 1]
    return $ do
        c <- [0..cols - 1]
        return . fromJust $ lab gr (fst $ mkNode_ nm (r, c))


printOption :: Option (Maybe Char) -> String
printOption o = unlines (fmap ((fromMaybe 'X') . R.index o) <$>
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
type SuperPos a = S.Set a


collectSuperPositions :: Gr Char Dir -> S.Set Position
collectSuperPositions gr
  = S.fromList . catMaybes $ toPosition <$> allNodes
    where
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
