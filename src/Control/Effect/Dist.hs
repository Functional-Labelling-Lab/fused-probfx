{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes #-}

{- | The effect for primitive distributions.
-}

module Control.Effect.Dist (
  -- ** Dist effect
    Dist(..)
  , dist
  ) where

import           Control.Algebra (Has, send)
import           Data.Kind       (Type)
import           PrimDist        (Addr, PrimDist, Tag)

-- | The effect @Dist@ for primitive distributions
data Dist (m :: Type -> Type) (k :: Type) where
    Dist :: PrimDist k -- ^ primitive distribution
         -> Maybe k    -- ^ optional observed value
         -> Maybe Tag  -- ^ optional observable variable name
         -> Dist m k

dist ::  PrimDist k -> Maybe k -> Maybe Tag -> ((Has Dist sig m) => m k)
dist primDist obs tag = send $ Dist primDist obs tag
