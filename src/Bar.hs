{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

{- | A coin-flip model for demonstrating how primitive distributions work in ProbFX.
-}

import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), bernoulli, uniform, handleCore )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Observables, nil, (<:>), Assign (..) )
import Prog (run)
import Inference.SIM (runSimulate)
import Sampler (sampleIO)

{- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
     distribution, and uses this to draw a boolean @y@ representing heads or tails.
-}
coinFlip
  :: (Observables env '["p"] Double
    , Observables env '[ "y"] Bool)
  => Model env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y


main :: IO ()
main = do
  let env = #p := [0.5] <:> #y := [] <:> nil

  let loop :: Int -> IO [Bool]
      loop 0 = do
        return []
      loop n = do
        (x, _) <- sampleIO $ runSimulate coinFlip env
        xs <- loop (n - 1)
        return $ x : xs

  xs <- loop 100

  print (length $ filter id xs)

