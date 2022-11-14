{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- | A coin-flip model for demonstrating how primitive distributions work in ProbFX.
-}

module CoinFlip where

import Control.Algebra (Has, send)
import           Data.Kind         (Constraint)
-- import           Effects.Dist      (Dist (Dist))
-- import           Effects.ObsReader (ObsReader (Ask))
import           Env               (Observables)
import           Model             (Model, bernoulli, uniform)
import           PrimDist          (PrimDist (BernoulliDist, UniformDist))
import Control.Effect.ObsReader (ObsReader(Ask))
import Control.Effect.Draw (Draw(Draw))
-- import           Prog              (call)

{- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
     distribution, and uses this to draw a boolean @y@ representing heads or tails.
-}
coinFlip
  :: forall env sig m. (Observables env '["p"] Double
    , Observables env '[ "y"] Bool, Has (Model env) sig m)
  => m Bool
coinFlip = do
  p <- uniform @env 0 1 #p
  y <- bernoulli @env p #y
  return y

{- | A desugared version of the above coin-flip model, after inlining the functions
     @uniform@ and @bernoulli@.
-}
coinFlip'
  :: forall env sig m. (Observables env '["p"] Double
    , Observables env '[ "y"] Bool, Has (Model env) sig m)
  => m Bool
coinFlip' = do
  maybe_p  <- send (Ask @env #p)
  p        <- send (Draw (UniformDist 0 1) maybe_p (Just "p"))
  maybe_y  <- send (Ask @env #y)
  y        <- send (Draw (BernoulliDist p) maybe_y (Just "p") )
  return y
