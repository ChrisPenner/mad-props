{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
module WFC.Class where

class WFCGraph (g :: * -> *) where
  type Edge g
  mapNodes :: (a -> b) -> g a -> g b
  default mapNodes :: Functor g => (a -> b) -> g a -> g b
  mapNodes = fmap
  edgesFrom


-- newGraph :: forall k e a. (Eq k, Hashable k, Hashable e, Eq e) => [(k, a)] -> [(k, k, e)] -> Graph k e a
-- newGraph vertexKeys edgeList = Graph vs es ls
--   where
--     vs :: IM.IntMap a
--     vs = IM.fromList $ fmap (first (fromJust . (HM.lookup ?? ls))) vertexKeys
--     es :: IM.IntMap (HM.HashMap e Vertex)
--     es = IM.unionsWith HM.union $ fmap toEdge edgeList
--     toEdge :: (k, k, e) -> IM.IntMap (HM.HashMap e Vertex)
--     toEdge (HM.lookup ?? ls -> Just from', HM.lookup ?? ls -> Just to', e) = IM.singleton from' (HM.singleton e to')
--     toEdge _ = mempty
--     ls :: HM.HashMap k Int
--     ls = HM.fromList (zip (fst <$> vertexKeys) [0..])

-- valueAt :: Vertex -> Traversal' (Graph k e a) a
-- valueAt n = values . ix n
-- {-# INLINE valueAt #-}

-- hmAsList :: (Eq k, Hashable k) => Iso' (HM.HashMap k v ) [(k, v)]
-- hmAsList = iso HM.toList HM.fromList
-- {-# INLINABLE hmAsList #-}

-- imAsList :: Iso' (IM.IntMap v ) [(Vertex, v)]
-- imAsList = iso IM.toList IM.fromList
-- {-# INLINABLE imAsList #-}

-- edgesFrom :: (Eq e, Hashable e) => Vertex -> Traversal' (Graph k e a) (e, Vertex)
-- edgesFrom n = edges . ix n . hmAsList . traversed
-- {-# INLINE edgesFrom #-}

-- vertexAt :: (Eq k, Hashable k) => k -> Fold (Graph k e a) Vertex
-- vertexAt k = labels . folding (HM.lookup k)
-- {-# INLINE vertexAt #-}

-- valueAtKey :: (Eq k, Hashable k) => k -> Traversal' (Graph k e a) a
-- valueAtKey k f graph' =
--     case vertex of
--         Nothing -> pure graph'
--         Just v -> graph' & valueAt v %%~ f
--     where
--       vertex = graph' ^? vertexAt k
-- {-# INLINE valueAtKey #-}
