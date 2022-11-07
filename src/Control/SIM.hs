{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- | Simulation.
-}

module Control.Inference.SIM (
    simulate
  , runSimulate
  , traceSamples
  , handleSamp
  , handleObs) where

import Data.Map (Map)
import Control.Effect.Dist (  Dist )
import Control.Effect.Sample ( Sample(..) )
import Control.Effect.Observe ( Observe(..) )
import Control.Effect.ObsReader ( ObsReader )
import Control.Effect.State ( State, modify )
import Control.Carrier.State.Strict ( runState )
import Env ( Env )
import Control.Model ( Model, handleCore )
import OpenSum (OpenSum)
import PrimDist ( pattern PrimDistPrf, sample )
import Prog ( Member(prj), Prog(..), discharge )
import qualified Data.Map as Map
import qualified OpenSum
import Sampler ( Sampler )
import Trace ( FromSTrace(..), STrace, updateSTrace )
import Unsafe.Coerce (unsafeCoerce)
import Control.Algebra (Has)

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

-- | Trace sampled values for each @Sample@ operation
traceSamples :: (Has (State STrace) sig m, Has Sample sig m) => m a -> m a
traceSamples (Val x) = return x
traceSamples (Op op k) = case prj op of
  Just (Sample (PrimDistPrf d) α) ->
       Op op (\x -> do modify (updateSTrace α d x);
                       traceSamples (k x))
  Nothing -> Op op (traceSamples . k)

-- | Handle @Sample@ operations by using the @Sampler@ monad to draw from primitive distributions
handleSamp :: Prog '[Sample] a -> Sampler a
handleSamp  (Val x)  = return x
handleSamp  (Op op k) = case discharge op of
  Right (Sample (PrimDistPrf d) α) ->
    do  x <- sample d
        handleSamp (k x)
  _        -> error "Impossible: Nothing cannot occur"
