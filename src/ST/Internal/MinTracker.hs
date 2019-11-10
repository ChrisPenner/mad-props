{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module ST.Internal.MinTracker where

import qualified Data.IntPSQ as PSQ
import Control.Monad.State
import Control.Lens as L
import ST.Internal.Graph

data MinTracker =
    MinTracker { _minQueue :: (PSQ.IntPSQ Int Vertex) }

makeLenses ''MinTracker

empty :: MinTracker
empty = MinTracker PSQ.empty

popMinNode :: MonadState MinTracker m => m (Maybe Vertex)
popMinNode = do
    minQueue %%= PSQ.alterMin \case
      Nothing -> (Nothing, Nothing)
      Just (_key, _priority, v) -> (Just v, Nothing)
{-# INLINE popMinNode #-}

setNodeEntropy :: MonadState MinTracker m => Vertex -> Int -> m ()
setNodeEntropy v ent = do
    minQueue %= snd . PSQ.insertView (v ^. vertexID) ent v
{-# INLINE setNodeEntropy #-}

fromList :: [(Vertex, Int)] -> MinTracker
fromList xs = MinTracker (PSQ.fromList (fmap assoc xs))
  where
    assoc (v, ent) = (v ^. vertexID, ent, v)
