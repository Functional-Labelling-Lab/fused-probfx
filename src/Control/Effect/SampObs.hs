{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for sampling and observing variables
-}

module Control.Effect.SampObs (
  -- ** SampObs effect
    SampObs(..)
  , sampObs
  ) where

import           Control.Algebra (Has, send)
import           Data.Kind       (Type)
import           PrimDist        (Addr, PrimDist, Tag)

-- | The effect @SampObs@ for handling a draw from a random variable
data SampObs (m :: Type -> Type) (k :: Type) where
    SampObs :: PrimDist k    -- ^ distribution to draw from
            -> Maybe k       -- ^ (possibly) observed value
            -> Addr          -- ^ address of @SampObs@ operation
            -> SampObs m k

-- | Smart Constructor for the 'SampObs' effect
sampObs :: (Has SampObs sig m)
  => PrimDist k -- ^ distribution to draw from
  -> Maybe k    -- ^ (possibly) observed value
  -> Addr       -- ^ address of @SampObs@ operation
  -> m k        -- ^ the sampled/observed value
sampObs d obs addr = send $ SampObs d obs addr
