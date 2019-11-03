{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Graph where

import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Lens
import Data.Maybe
import Data.Bifunctor

type Vertex = Int

data Graph k e a =
    Graph { _values :: IM.IntMap a
          , _edges  :: IM.IntMap (IM.IntMap e)
          , _labels :: HM.HashMap k Int
          } deriving (Functor, Foldable, Traversable)

makeLenses ''Graph

instance Bifunctor (Graph k) where
  second = fmap
  first f = edges . mapped . mapped %~ f

newGraph :: forall k e a. (Eq k, Hashable k) => [(k, a)] -> [(k, k, e)] -> Graph k e a
newGraph vertexKeys edgeList = Graph vs es ls
  where
    vs :: IM.IntMap a
    vs = IM.fromList $ fmap (first (fromJust . (HM.lookup ?? ls))) vertexKeys
    es :: IM.IntMap (IM.IntMap e)
    es = IM.unionsWith IM.union $ fmap toEdge edgeList
    toEdge :: (k, k, e) -> IM.IntMap (IM.IntMap e)
    toEdge (HM.lookup ?? ls -> Just from', HM.lookup ?? ls -> Just to', e) = IM.singleton from' (IM.singleton to' e)
    toEdge _ = error "node found in edge list which is missing in vertex list"
    ls :: HM.HashMap k Int
    ls = HM.fromList (zip (fst <$> vertexKeys) [0..])

value :: Vertex -> Traversal' (Graph k e a) a
value n = values . ix n

hmAsList :: (Eq k, Hashable k) => Iso' (HM.HashMap k v ) [(k, v)]
hmAsList = iso HM.toList HM.fromList

imAsList :: Iso' (IM.IntMap v ) [(Vertex, v)]
imAsList = iso IM.toList IM.fromList

edgesFrom :: Vertex -> Traversal' (Graph k e a) (Vertex, e)
edgesFrom n = edges . ix n . imAsList . traversed

vertexOf :: (Eq k, Hashable k) => k -> Fold (Graph k e a) Vertex
vertexOf k = labels . folding (HM.lookup k)
