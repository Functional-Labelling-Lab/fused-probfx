{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module FusedEffects.ObsReader 
  (ObsReader(..))
  where

import Env (Observable, ObsVar)
import Data.Kind (Type)
import Control.Algebra ( Has )


data ObsReader env (m :: Type -> Type) k where
  -- | Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
  Ask :: Observable env x a
    => ObsVar x                     -- ^ variable @x@ to read from
    -> ObsReader env m (Maybe a)    -- ^ the head value from @x@'s list

ask :: (Has (ObsReader env) sig m, Observable env x a) => ObsVar x -> m (Maybe a)
ask = undefined
