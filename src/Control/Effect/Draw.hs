{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}

{- | The effect for primitive distributions.
-}

module Control.Effect.Draw (
  -- ** Draw effect
    Draw(..)
  , draw
  ) where

import           Control.Algebra (Has, send)
import           Data.Kind       (Type)
import           PrimDist        (Addr, PrimDist, Tag)

-- | The effect @Dist@ for primitive distributions
data Draw (m :: Type -> Type) (k :: Type) where
    Draw :: PrimDist k -- ^ primitive distribution
         -> Maybe k    -- ^ optional observed value
         -> Maybe Tag  -- ^ optional observable variable name
         -> Draw m k

draw :: (Has Draw sig m) => PrimDist k -> Maybe k -> Maybe Tag -> m k
draw primDist obs tag = send $ Draw primDist obs tag
