{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module WFC.MinTracker where

import qualified Data.IntPSQ as PSQ
import Control.Monad.State
import Control.Lens as L
import WFC.Graph as G

data MinTracker =
    MinTracker { _minQueue :: (PSQ.IntPSQ Int ()) }

makeLenses ''MinTracker

empty :: MinTracker
empty = MinTracker PSQ.empty

popMinNode :: MonadState MinTracker m => m (Maybe G.Vertex)
popMinNode = do
    use (minQueue . to PSQ.minView) >>= \case
      Nothing -> return Nothing
      Just (n, _, _, q) -> do
          minQueue .= q
          return $ Just n

setNodeEntropy :: MonadState MinTracker m => G.Vertex -> Int -> m ()
setNodeEntropy nd ent = do
    minQueue %= snd . PSQ.insertView nd ent ()

fromList :: [(G.Vertex, Int)] -> MinTracker
fromList xs = MinTracker (PSQ.fromList (fmap assoc xs))
  where
    assoc (n, ent) = (n, ent, ())
