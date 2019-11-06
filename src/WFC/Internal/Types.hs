{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module WFC.Internal.Types where

import Control.Lens as L

data SuperPos f a =
    Observed a | Unknown (f a)
  deriving (Show, Foldable)

makePrisms ''SuperPos

type PropFilter f e a = f a -> e -> (f a -> Bool)

fromObserved :: SuperPos f a -> a
fromObserved (Observed a) = a
fromObserved (Unknown _) = error "fromObserved Error!"

-- superPosFilter :: (s -> Bool) -> SuperPos s -> Backtrack (SuperPos s)
-- superPosFilter _ o@(Observed{}) = return o
-- superPosFilter p (Unknown s) =
--     maybe backtrack (pure . Unknown) . NE.nonEmptySet $ NE.filter p s
