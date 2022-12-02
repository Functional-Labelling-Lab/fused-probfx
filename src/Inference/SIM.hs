{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{- | Simulation.
-}

module Inference.SIM (
    simulate
  , runSimulate, SampObsC, runSampObs) where
import           Control.Algebra            (Algebra (alg), Has, (:+:) (L, R))
import           Control.Carrier.Dist       (DistC, runDist)
import           Control.Carrier.Lift       (LiftC, runM)
import           Control.Carrier.ObsReader  (ObsReaderC, runObsReader)
import           Control.Carrier.SampTracer (SampTracerC, runSampTracer)
import           Control.Effect.Lift        (Lift, sendM)
import           Control.Effect.SampObs     (SampObs (..))
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Env                        (Env)
import           PrimDist                   (dist)
import           Control.Effect.Sampler                    (Sampler)
import           Trace                      (FromSTrace (fromSTrace), STrace)

-- SampObs Effect Carrier
newtype SampObsC (m :: * -> *) (k :: *) = SampObsC { runSampObs :: m k }
    deriving (Functor, Applicative, Monad)

-- SampObsC replaces the SampObs effect with the Lift Sampler effect
instance (Has Sampler sig m) => Algebra (SampObs :+: sig) (SampObsC m) where
  alg hdl sig ctx = SampObsC $ case sig of
    L (SampObs d obs addr) -> do
      x <- case obs of
        Just v -> pure v
        Nothing -> dist d
      -- x <- sendM $ maybe (dist d) pure obs
      pure $ x <$ ctx
    R other -> alg (runSampObs . hdl) other ctx

-- | Top-level wrapper for simulating from a model
simulate :: (FromSTrace env, Has Sampler sig m)
  => Env env                                                     -- ^ model environment
  -> ObsReaderC env (DistC (SampTracerC (SampObsC m))) a -- ^ model
  -> m (a, Env env)                                        -- ^ (model output, output environment)
simulate model env = do
  (output, strace) <- runSimulate model env
  return (output, fromSTrace strace)

-- | Handler for simulating once from a probabilistic program
runSimulate :: (Has Sampler sig m)
 => Env env                                                     -- ^ model environment
 -> ObsReaderC env (DistC (SampTracerC (SampObsC m))) a -- ^ model
 -> m (a, STrace)                                         -- ^ (model output, sample trace)
runSimulate env
  = runSampObs . runSampTracer . runDist . runObsReader env
