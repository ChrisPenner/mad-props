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
    | C
    deriving (Eq, Show, Ord)

type N loc val = (loc, val)

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

rectangularize :: T.Text -> (Row, Col, [T.Text])
rectangularize txt = (length lines', maxLength, padded)
  where
    lines' = T.lines txt
    maxLength = maximum . fmap T.length $ lines'
    padded = T.justifyLeft maxLength ' ' <$> lines'

testText :: T.Text
testText = [R.r|one
a longer thing
there
|]

simpleText :: T.Text
simpleText =
  [R.r|......
......
......
|]


when_ :: Monad m => Bool -> m a -> m ()
when_ cond = when cond . void

type SuperPos a = S.Set a
type Position = Option (Maybe Char)

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

collectSuperPositions :: Gr Char Dir -> S.Set Position
collectSuperPositions gr
  = S.fromList $ toPosition <$> allNodes
    where
      allNodes = nodes gr
      toPosition :: Node -> Position
      toPosition n =
          let linked = context gr n
           in tabulate (findEdge linked)
      findEdge :: Context Char Dir -> Dir -> Maybe Char
      findEdge ctx C = Just $ lab' ctx
      findEdge ctx dir = do
          (n, _) <- L.find ((== dir) . snd) $ lsuc' ctx
          lab gr n

generateGrid :: p -> Row -> Col -> (NodeMap Coord, Gr p Dir)
generateGrid positions row col = (nodemap, nmap (const positions) gr)
    where
      (nodemap, gr) = mkGridGraph row col

graphToText :: Row -> Col -> NodeMap Coord -> Gr Char e -> T.Text
graphToText rows cols nm gr = T.pack . unlines $ do
    r <- [0..rows - 1]
    return $ do
        c <- [0..cols - 1]
        return . fromJust $ lab gr (fst $ mkNode_ nm (r, c))

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
solve gr = case minWavinessNode gr of
    Nothing -> nmap pickFirst $ gr
    Just n -> solve $ collapse gridFilter n gr

pickFirst :: S.Set a -> a
pickFirst = S.elemAt 0

printOption :: Option (Maybe Char) -> String
printOption o = unlines (fmap ((fromMaybe 'X') . R.index o) <$>
    [ [NW, N, NE]
    , [W,  C, E ]
    , [SW, S, SE]])

gridFilter :: (Eq a) => Option a -> Dir -> (Option a -> Bool)
gridFilter choice dir other =
    R.index choice dir
      == R.index other (flipDir dir)

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

collapse :: forall p e. (p -> e -> (p -> Bool)) -> Node -> Gr (SuperPos p) e -> Gr (SuperPos p) e
collapse nFilter n gr = newGraph
  where
    newGraph = gr
        & ctxAt n . ctxLabel .~ S.singleton choice
        &~ for propTargets
             (\(e, n) -> ctxAt n . ctxLabel %= S.filter (propFilter e))
    propTargets :: [(e, Node)]
    propTargets = gr ^.. ctxAt n . ctxSuc . traversed
    choice :: p
    choice = gr ^?! ctxAt n . ctxLabel . folded
    propFilter = nFilter choice

simplePos =
    let (rows, cols, srcNM, srcGr) = graphFromText $ simpleText
     in collectSuperPositions srcGr


test :: IO ()
test = do
    let (rows, cols, srcNM, srcGr) = graphFromText $ simpleText
    let positions = collectSuperPositions srcGr
    -- T.putStrLn $ graphToText rows cols srcNM srcGr
    let (destNM, startGrid) = generateGrid positions 10 10
    let solved = nmap (fromJust . flip R.index C) $ solve startGrid
    T.putStrLn $ graphToText 10 10 destNM solved

    return ()
