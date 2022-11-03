{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for observing.
-}

module Control.Effect.Observe (
  -- ** Observe effect
  Observe(..)
  ) where

import PrimDist (PrimDist, Tag, Addr)
import Data.Kind (Type)

-- | The effect @Observe@ for conditioning against observed values
data Observe a (m :: Type -> Type) (k :: Type) where
    Observe :: PrimDist a    -- ^ distribution to condition with
            -> a             -- ^ observed value
            -> Addr          -- ^ address of @Observe@ operation
            -> Observe a m k
