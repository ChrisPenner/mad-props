{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module WFC.Types where

import WFC
import qualified Data.Set.NonEmpty as NE
import Control.Lens as L

data SuperPos a =
    Observed a | Unknown (NE.NESet a)
  deriving (Show, Foldable)

makePrisms ''SuperPos

type PropFilter f e a = f a -> e -> (f a -> Bool)

fromObserved :: SuperPos a -> a
fromObserved (Observed a) = a
fromObserved (Unknown _) = error "fromObserved Error!"

superPosFilter :: (s -> Bool) -> SuperPos s -> WFC (SuperPos s)
superPosFilter _ o@(Observed{}) = return o
superPosFilter p (Unknown s) =
    maybe backtrack (pure . Unknown) . NE.nonEmptySet $ NE.filter p s
