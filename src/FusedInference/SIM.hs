{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Intference.SIM2 (
    simulate
  , runSimulate
  , traceSamples
  , handleSamp
  , handleObs
) where

import Trace ( FromSTrace, STrace )
import Control.Algebra ( Has )
import Control.Effect.State ( State )
import Model (Model)
import Env (Env)
import Sampler (Sampler)
import qualified Data.Map as Map

-- | Top-level wrapper for simulating from a model
simulate :: (FromSTrace env,
            Has (ObsReader env) sig m
            Has (State STrace) sig m,
            Has Observe sig m,
            Has Sample sig m)
            => Model env sig a       -- ^ model
            -> Env env               -- ^ model environment
            -> Sampler (a, Env, env) -- ^ (model output, output environment)
simulate model env = do
  (output, strace) <- runSimulate model env
  return (output, fromSTrace strace)

runSimulate :: (FromSTrace env,
               Has (ObsReader env) sig m
               Has (State STrace) sig m,
               Has Observe sig m,
               Has Sample sig m)
               => Model env sig a       -- ^ model
               -> Env env               -- ^ model environment
               -> Sampler (a, Env, env) -- ^ (model output, output environment)
runSimulate model
  = handleSamp . handleObs . handleState Map.empty . traceSamples . handleCore model

traceSamples :: a
traceSamples = undefined

handleSamp :: a
handleSamp = undefined

handleObs :: a
handleObs = undefined
