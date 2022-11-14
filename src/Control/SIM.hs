{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | Simulation.
-}

module Control.Inference.SIM (
    simulate
  , runSimulate) where

import           Control.Algebra              (Has)
import           Control.Carrier.State.Strict (runState)
import           Control.Effect.Dist          (Dist)
import           Control.Effect.Observe       (Observe (..))
import           Control.Effect.ObsReader     (ObsReader)
import           Control.Effect.Sample        (Sample (..))
import           Control.Effect.State         (State, modify)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Env                          (Env)
import           OpenSum                      (OpenSum)
import qualified OpenSum
import           PrimDist                     (pattern PrimDistPrf, sample)
import           Prog                         (Member (prj), Prog (..),
                                               discharge)
import           Sampler                      (Sampler)
import           Trace                        (FromSTrace (..), STrace,
                                               updateSTrace)
import           Unsafe.Coerce                (unsafeCoerce)

-- | Top-level wrapper for simulating from a model
simulate :: (FromSTrace env, Has (ObsReader env) sig m, Has Dist sig m, Has (State STrace) sig m, Has Observe sig m, Has Sample sig m)
  => Model env sig m a       -- ^ model
  -> Env env              -- ^ model environment
  -> Sampler (a, Env env) -- ^ (model output, output environment)
simulate model env = do
  (output, strace) <- runSimulate model env
  return (output, fromSTrace strace)

-- | Handler for simulating once from a probabilistic program
runSimulate :: (Has (ObsReader env) sig m, Has Dist sig m, Has (State STrace) sig m, Has Observe sig m, Has Sample sig m)
 => Model env sig m a      -- ^ model
 -> Env env             -- ^ model environment
 -> Sampler (a, STrace) -- ^ (model output, sample trace)
runSimulate model
  = handleSamp . handleObs . handleState Map.empty . traceSamples . handleCore model
