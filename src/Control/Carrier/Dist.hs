{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Control.Carrier.Dist (
    DistC(..),
    runDist
) where

import Control.Algebra
    ( Has, Algebra(..), type (:+:)(..), Handler, send )
import Control.Effect.Dist (Dist(..))
import Control.Effect.Sample (Sample(..))
import Control.Effect.Observe (Observe)
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Effects.Dist (Tag)
import Control.Effect.Observe (Observe(..))
import qualified Control.Effect.State as State
import Control.Carrier.State.Strict (StateC, runState, evalState)
import Control.Algebra (run)
import Data.Functor.Identity (Identity)

newtype DistC a m k = DistC { runDistC :: StateC (Int, Map.Map Tag Int) m k }
    deriving (Functor, Applicative, Monad)

runDist :: Functor m => DistC a m b -> m b
runDist = evalState (0, Map.empty) . runDistC

instance (Algebra sig m, Has (Sample b) sig m, Has (Observe b) sig m) => Algebra (Dist b :+: sig) (DistC b m) where
  alg hdl sig ctx = DistC $ case sig of
    L (Dist primDist mObs mTag) -> do
      -- Get current counter and tagMap
      (counter, tagMap) <- State.get @(Int, Map.Map Tag Int) 

      -- Calculate tag and tagIdx to pass to the sample/observe
      let tag = fromMaybe (show counter) mTag
      let tagIdx = Map.findWithDefault 0 tag tagMap

      -- Increment the counter and the address
      let counter' = counter + 1
      let tagMap' = Map.insert tag (tagIdx + 1) tagMap
      State.put (counter', tagMap')

      x <- case mObs of
        -- Variable to observe from set, replace this dist with an observe call
        Just obs -> send $ Observe primDist obs (tag, tagIdx)
        -- No value to observe supplied, replace dist with 
        Nothing -> send $ Sample primDist (tag, tagIdx)

      pure $ x <$ ctx

    R other -> alg (runDistC . hdl) (R other) ctx
