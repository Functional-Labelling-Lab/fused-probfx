{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for observing.
-}

module Control.Effect.SampObs (
  -- ** SampObs effect
    SampObs(..)
  , sampObs
  ) where

import           Control.Algebra (Has, send)
import           Data.Kind       (Type)
import           PrimDist        (Addr, PrimDist, Tag)

-- | The effect @Dist@ for handling a draw from a specific distribution
data SampObs (m :: Type -> Type) (k :: Type) where
    SampObs :: PrimDist k       -- ^ distribution to draw from
            -> Maybe k       -- ^ (possibly) observed value
            -> Addr          -- ^ address of @Dist@ operation
            -> SampObs m k

sampObs :: (Has SampObs sig m) => PrimDist k -> Maybe k -> Addr -> m k
sampObs d obs addr = send $ SampObs d obs addr
