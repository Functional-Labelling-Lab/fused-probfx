{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- | The effect for reading observable variables from a model environment.
-}

module Control.Effect.ObsReader (
    ObsReader(..),
    ask
  ) where

import           Control.Algebra
import           Data.Kind       (Type)
import           Data.Maybe      (listToMaybe)
import           Env             (Assign, Env, ObsVar, Observable (..))
import           GHC.Types       (Symbol)

-- | The effect for reading observed values from a model environment @env@
data ObsReader (env :: [Assign Symbol *]) (m :: Type -> Type) (k :: Type) where
  -- | Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
  Ask :: Observable env x a
    => ObsVar x                   -- ^ variable @x@ to read from
    -> ObsReader env m (Maybe a)  -- ^ the head value from @x@'s list

ask :: forall env sig m x a. (Has (ObsReader env) sig m, Observable env x a)
  => ObsVar x
  -> m (Maybe a)
ask x = send (Ask @env x)
