{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeOperators    #-}

{- | Likelihood-Weighting inference.
-}

module Inference.LW (
    lw
  , runLW) where

import           Control.Algebra            (Algebra (alg))
import           Control.Carrier.Dist       (DistC, runDist)
import           Control.Carrier.Lift       (LiftC, runM)
import           Control.Carrier.LPTracer   (LPTracerC, runLPTracer)
import           Control.Carrier.ObsReader  (ObsReaderC, runObsReader)
import           Control.Carrier.SampTracer (SampTracerC, runSampTracer)
import           Control.Effect.SampObs     (SampObs (SampObs))
import           Control.Monad              (replicateM)
import qualified Data.Map                   as Map (filterWithKey, foldr, (!?))
import           Data.Maybe                 (isJust)
import           Env                        (Env)
import           Inference.SIM              (SampObsC, runSampObs)
import           PrimDist                   (logProb)
import           Sampler                    (Sampler)
import           Trace                      (FromSTrace (..), STrace)

-- | Top-level wrapper for Likelihood-Weighting (LW) inference
lw :: (FromSTrace env)
    => Int            -- ^ number of LW iterations
    -> Env env        -- ^ model environment
    -> ObsReaderC env (DistC (SampTracerC (LPTracerC (SampObsC (LiftC Sampler))))) a -- ^ model
    -> Sampler [(Env env, Double)] -- ^ [(output model environment, likelihood-weighting)]
lw n env model = do
  lwTrace <- replicateM n (runLW env model)
  return $ map (\((_, strace), p) -> (fromSTrace strace, p)) lwTrace

-- | Handler for one iteration of LW
runLW ::
     Env env        -- ^ model environment
  -> ObsReaderC env (DistC (SampTracerC (LPTracerC (SampObsC (LiftC Sampler))))) a -- ^ model
  -> Sampler ((a, STrace), Double) -- ^ ((model output, zsample trace), likelihood-weighting)
runLW env m = do
    ((output, strace), lptrace) <- runM $ runSampObs $ runLPTracer False $ runSampTracer $ runDist $ runObsReader env m
    let l = exp $ Map.foldr (+) 0 lptrace
    return ((output, strace), l)
