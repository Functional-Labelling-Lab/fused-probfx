{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for sampling.
-}

module Control.Effect.Sample (
  -- ** Sample effect
    Sample(..)
  , sample
  ) where

import Control.Algebra (Has, send)
import PrimDist (PrimDist, Tag, Addr)
import Data.Kind (Type)

-- | The effect @Sample@ for sampling from distirbutions
data Sample (m :: Type -> Type) (k :: Type) where
    Sample :: PrimDist k   -- ^ distribution to sample from
           -> Addr         -- ^ address of @Sample@ operation
           -> Sample m k

sample :: (Has Sample sig m) => PrimDist k -> Addr -> m k
sample dist addr = send (Sample dist addr)
