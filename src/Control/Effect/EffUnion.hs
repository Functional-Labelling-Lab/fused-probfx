{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The effect for reading observable variables from a model environment.
-}

module Control.Effect.EffUnion (
    EffUnion(..),
    FlipEffect(..)
  ) where

import           Control.Algebra
import           Data.Kind       (Type)
import           Data.Maybe      (listToMaybe)
import           Env             (Assign (..), ObsVar)
import           GHC.Types       (Symbol)
import Control.Effect.Sum (Member (inj))
import qualified Data.WorldPeace.Union as Union

newtype FlipEffect (e :: u -> (* -> *) -> * -> *) (m :: * -> *) (k :: *) (a :: u) = FlipEffect {runFlipEffect :: e a m k}

-- | The effect for reading observed values from a model environment @env@
data EffUnion (e :: u -> (* -> *) -> * -> *) (as :: [u]) (m :: Type -> Type) (k :: Type) where
  EffUnion :: forall as e m k. (Union.Union (FlipEffect e m k) as -> EffUnion e as m k)


instance {-# INCOHERENT #-} (Member (EffUnion e as) sig) => Member (e a) sig where
  inj eff = undefined