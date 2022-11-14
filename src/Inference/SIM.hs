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
{-# LANGUAGE UndecidableInstances #-}

{- | Simulation.
-}

module Inference.SIM (
    simulate
  , runSimulate, DistC, runDist) where
import           Control.Algebra              (Has, Algebra (alg), (:+:) (L, R))
import           Control.Carrier.Draw         (DrawC, runDraw)
import           Control.Carrier.ObsReader    (ObsReaderC, runObsReader)
import           Control.Carrier.SampleTracer (SampleTracerC, runSampleTracer)
import           Control.Effect.Dist          (Dist (..))
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Env                          (Env)
import           PrimDist                     (draw)
import           Sampler                      (Sampler)
import           Trace                        (FromSTrace (fromSTrace), STrace)
import Control.Effect.Lift (Lift)
import Control.Carrier.Lift (LiftC, runM)
import Control.Effect.Lift (sendM)

-- Dist Effect Carrier
newtype DistC (m :: * -> *) (k :: *) = DistC { runDist :: m k }
    deriving (Functor, Applicative, Monad)

instance (Has (Lift Sampler) sig m) => Algebra (Dist :+: sig) (DistC m) where
  alg hdl sig ctx = DistC $ case sig of
    L (Dist d obs addr) -> do
      x <- sendM $ maybe (draw d) pure obs
      pure $ x <$ ctx
    R other -> alg (runDist . hdl) other ctx

-- | Top-level wrapper for simulating from a model
simulate :: (FromSTrace env)
  => Env env                                                     -- ^ model environment
  -> ObsReaderC env (DrawC (SampleTracerC (DistC (LiftC Sampler)))) a -- ^ model
  -> Sampler (a, Env env)                                        -- ^ (model output, output environment)
simulate model env = do
  (output, strace) <- runSimulate model env
  return (output, fromSTrace strace)

-- | Handler for simulating once from a probabilistic program
runSimulate ::
    Env env                                                     -- ^ model environment
 -> ObsReaderC env (DrawC (SampleTracerC (DistC (LiftC Sampler)))) a -- ^ model
 -> Sampler (a, STrace)                                         -- ^ (model output, sample trace)
runSimulate env
  = runM . runDist . runSampleTracer . runDraw . runObsReader env
