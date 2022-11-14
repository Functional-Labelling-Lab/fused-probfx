{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for observing.
-}

module Control.Effect.Dist (
  -- ** Dist effect
    Dist(..)
  , dist
  ) where

import           Control.Algebra (Has, send)
import           Data.Kind       (Type)
import           PrimDist        (Addr, PrimDist, Tag)

-- | The effect @Dist@ for handling a draw from a specific distribution
data Dist (m :: Type -> Type) (k :: Type) where
    Dist :: PrimDist k       -- ^ distribution to draw from
            -> Maybe k       -- ^ (possibly) observed value
            -> Addr          -- ^ address of @Dist@ operation
            -> Dist m k

dist :: (Has Dist sig m) => PrimDist k -> Maybe k -> Addr -> m k
dist d obs addr = send $ Dist d obs addr
