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
data Observe (m :: Type -> Type) (k :: Type) where
    Observe :: PrimDist k    -- ^ distribution to condition with
            -> k             -- ^ observed value
            -> Addr          -- ^ address of @Observe@ operation
            -> Observe m k
