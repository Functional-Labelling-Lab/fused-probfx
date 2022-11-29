{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Simulation.
-}

module Inference.SIM (
    simulate
  , runSimulate, SampObsC, runSampObs) where
import           Control.Algebra            (Algebra (alg), Has, (:+:) (L, R))
import           Control.Carrier.Dist       (DistC, runDist)
import           Control.Carrier.Lift       (LiftC, runM)
import           Control.Carrier.ObsReader  (ObsReaderC, runObsReader)
import           Control.Carrier.Product    (ProductC, runProduct)
import           Control.Carrier.SampTracer (SampTracerC, runSampTracer)
import           Control.Effect.Lift        (Lift, sendM)
import           Control.Effect.SampObs     (SampObs (..))
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Env                        (Env, EnvElem (Elem))
import           PrimDist                   (dist)
import           Sampler                    (Sampler)
import           Trace                      (FromSTrace (fromSTrace), STrace)
import Control.Effect.ObsReader (ObsReader)

-- SampObs Effect Carrier
newtype SampObsC (m :: * -> *) (k :: *) = SampObsC { runSampObs :: m k }
    deriving (Functor, Applicative, Monad)

instance (Has (Lift Sampler) sig m) => Algebra (SampObs :+: sig) (SampObsC m) where
  alg hdl sig ctx = SampObsC $ case sig of
    L (SampObs d obs addr) -> do
      x <- sendM $ maybe (dist d) pure obs
      pure $ x <$ ctx
    R other -> alg (runSampObs . hdl) other ctx

-- | Top-level wrapper for simulating from a model
simulate :: (FromSTrace env)
  => Env env                                                      -- ^ model environment
  -> ProductC ObsReader ObsReaderC env (DistC (SampTracerC (SampObsC (LiftC Sampler)))) a -- ^ model
  -> Sampler (a, Env env)                                         -- ^ (model output, output environment)
simulate model env = do
  (output, strace) <- runSimulate model env
  return (output, fromSTrace strace)

-- | Handler for simulating once from a probabilistic program
runSimulate ::
    Env env                                                     -- ^ model environment
 -> ProductC ObsReader ObsReaderC env (DistC (SampTracerC (SampObsC (LiftC Sampler)))) a -- ^ model
 -> Sampler (a, STrace)                                         -- ^ (model output, sample trace)
runSimulate env
  = runM . runSampObs . runSampTracer . runDist . runProduct env (\(Elem vs) -> runObsReader vs)
