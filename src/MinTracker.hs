{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module MinTracker where

import qualified Data.IntPSQ as PSQ
import Data.Graph.Inductive hiding ((&))
import Control.Monad.State
import Control.Lens as L

data MinTracker =
    MinTracker { _minQueue :: (PSQ.IntPSQ Int ()) }

makeLenses ''MinTracker

empty :: MinTracker
empty = MinTracker PSQ.empty

popMinNode :: MonadState MinTracker m => m (Maybe Node)
popMinNode = do
    use (minQueue . to PSQ.minView) >>= \case
      Nothing -> return Nothing
      Just (n, _, _, q) -> do
          minQueue .= q
          return $ Just n

setNodeEntropy :: MonadState MinTracker m => Node -> Int -> m ()
setNodeEntropy nd ent = do
    minQueue %= snd . PSQ.insertView nd ent ()

fromList :: [(Node, Int)] -> MinTracker
fromList xs = MinTracker (PSQ.fromList (fmap assoc xs))
  where
    assoc (n, ent) = (n, ent, ())
