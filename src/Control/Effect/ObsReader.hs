{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- | The effect for reading observable variables from a model environment.
-}

module Control.Effect.ObsReader (
    ObsReader(..),
    ask
  ) where

import           Control.Algebra
import           Data.Kind       (Type)
import           Data.Maybe      (listToMaybe)
import           Env             (Assign (..), ObsVar)
import           GHC.Types       (Symbol)

-- | The effect for reading observed values from a model environment @env@
data ObsReader (e :: Assign Symbol Type) (m :: Type -> Type) (k :: Type) where
  -- | Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
  Ask :: ObsReader (x ':= a) m (Maybe a)  -- ^ the head value from @x@'s list

ask :: forall x a sig m. (Has (ObsReader (x ':= a)) sig m)
  => m (Maybe a)
ask = send (Ask @x)
