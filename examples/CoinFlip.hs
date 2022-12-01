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

import           Control.Algebra          (Has, send)
import           Control.Effect.Dist      (Dist (Dist))
import           Control.Effect.ObsReader (ObsReader (Ask))
import           Data.Kind                (Constraint)
import           Env                      (Observables)
import           Model                    (Model (Model), bernoulli, uniform)
import           PrimDist                 (PrimDist (BernoulliDist, UniformDist))


{- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
     distribution, and uses this to draw a boolean @y@ representing heads or tails.
-}
coinFlip
  :: (Observables env '["p"] Double
    , Observables env '[ "y"] Bool)
  => Model env sig m Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

{- | A desugared version of the above coin-flip model, after inlining the functions
     @uniform@ and @bernoulli@.
-}
coinFlip'
  :: forall env sig m. (Observables env '["p"] Double
    , Observables env '[ "y"] Bool)
  => Model env sig m Bool
coinFlip' = do
  maybe_p  <- Model $ send (Ask @env #p)
  p        <- Model $ send (Dist (UniformDist 0 1) maybe_p (Just "p"))
  maybe_y  <- Model $ send (Ask @env #y)
  y        <- Model $ send (Dist (BernoulliDist p) maybe_y (Just "p") )
  return y
