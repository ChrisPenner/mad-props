{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module GraphM where

newtype GraphM k e a = GraphM {runGraphM :: StateT (Graph k e Obscured) IO a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)


