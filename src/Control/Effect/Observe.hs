{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for observing.
-}

module Control.Effect.Observe (
  -- ** Observe effect
    Observe(..)
  , observe
  ) where

import           Control.Algebra (Has, send)
import           Data.Kind       (Type)
import           PrimDist        (Addr, PrimDist, Tag)

-- | The effect @Observe@ for conditioning against observed values
data Observe (m :: Type -> Type) (k :: Type) where
    Observe :: PrimDist k    -- ^ distribution to condition with
            -> k             -- ^ observed value
            -> Addr          -- ^ address of @Observe@ operation
            -> Observe m k

observe :: (Has Observe sig m) => PrimDist k -> k -> Addr -> m k
observe primDist obs addr = send $ Observe primDist obs addr
