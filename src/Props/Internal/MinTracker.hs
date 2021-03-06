{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Props.Internal.MinTracker where

import qualified Data.IntPSQ as PSQ
import Control.Monad.State
import Control.Lens as L
import Props.Internal.Graph

data MinTracker =
    MinTracker { _minQueue :: (PSQ.IntPSQ Int ()) }

makeClassy ''MinTracker

empty :: MinTracker
empty = MinTracker PSQ.empty

popMinNode :: (HasMinTracker e, MonadState e m) => m (Maybe Vertex)
popMinNode = do
    uses minQueue PSQ.minView >>= \case
      Nothing -> return Nothing
      Just (n, _, _, q) -> do
          minQueue .= q
          return $ Just (Vertex n)
{-# INLINE popMinNode #-}

setNodeEntropy :: (HasMinTracker e, MonadState e m) => Vertex -> Int -> m ()
setNodeEntropy (Vertex nd) ent = do
    minQueue %= snd . PSQ.insertView nd ent ()
{-# INLINE setNodeEntropy #-}

fromList :: [(Vertex, Int)] -> MinTracker
fromList xs = MinTracker (PSQ.fromList (fmap assoc xs))
  where
    assoc (Vertex n, ent) = (n, ent, ())
