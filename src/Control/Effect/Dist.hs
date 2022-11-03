{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}

{- | The effect for primitive distributions.
-}

module Control.Effect.Dist (
  -- ** Dist effect
    Dist(..)
  ) where

import PrimDist (PrimDist, Tag, Addr)
import Data.Kind (Type)

-- | The effect @Dist@ for primitive distributions
data Dist (m :: Type -> Type) (k :: Type) where
    Dist :: PrimDist k -- ^ primitive distribution
         -> Maybe k    -- ^ optional observed value
         -> Maybe Tag  -- ^ optional observable variable name
         -> Dist m k
