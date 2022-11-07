{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

{- | A coin-flip model for demonstrating how primitive distributions work in ProbFX.
-}

import Control.Effect.ObsReader ( ObsReader(Ask), ask )
import Control.Effect.State ( State(..))
import Control.Model ( Model(Model), bernoulli, uniform, handleCore )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Control.Effect.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Observables, nil, (<:>), Assign (..), Observable, ObsVar, Env )
import Inference.SIM (runSimulate, simulate)
import Sampler (sampleIO)
import Control.Algebra (Has, send)
import Control.Algebra (Has, send, Algebra (alg), type (:+:) (L), run)
import Control.Effect.Sum (type (:+:)(R), Member)
import Control.Carrier.ObsReader (ObsReaderC, runObsReader)
import Control.Effect.State (State)
import Control.Effect.State (get)
import Control.Carrier.Dist (runDist)
import Control.Carrier.Observe (ObserveC(..))
import Control.Carrier.Sample (SampleC(runSampleC))


-- import Prog ( call )
-- import Effects.ObsReader ( ObsReader(Ask) )
-- import Model ( Model(Model), bernoulli, uniform, handleCore )
-- import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
-- import Effects.Dist ( Dist(Dist) )
-- import Data.Kind (Constraint)
-- import Env ( Observables, nil, (<:>), Assign (..), Env )
-- import Prog (run)
-- import Inference.SIM (runSimulate, simulate, handleSamp, handleObs, traceSamples)
-- import Sampler (sampleIO)
-- import qualified Data.Map as Map
-- import Effects.State (handleState)

{- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
     distribution, and uses this to draw a boolean @y@ representing heads or tails.
-}
-- coinFlip
--   :: (Observables env '[ "y"] Bool)
--   => Model env es Bool
-- coinFlip = do
--   y <- bernoulli 0.5 #y
--   return y

coinFlip
  :: forall env sig m. (
    Observables env '[ "y"] Bool,
    Has Dist sig m,
    Has (ObsReader env) sig m)
  => Model env sig m Bool
coinFlip = Model $ do
  maybe_y  <- send (Ask @env #y)
  y        <- send (Dist (BernoulliDist 0.5) maybe_y (Just "p") )
  return y


main :: IO ()
main = do
  let env :: Env '["y" ':= Bool]
      env = #y := [] <:> nil

  let loop :: Int -> IO [Bool]
      loop 0 = do
        return []
      loop n = do
        x <- sampleIO $ runSampleC $ runObserveC $ handleCore (coinFlip @('["y" ':= Bool])) env
        xs <- loop (n - 1)
        return $ x : xs

  xs <- loop 100

  print (length $ filter id xs)




-- -- TYPICAL PROBFX COINFLIP

-- {- | A coin-flip model that draws a coin-bias @p@ between 0 and 1 from a uniform
--      distribution, and uses this to draw a boolean @y@ representing heads or tails.
-- -}
-- coinFlip
--   :: (Observables env '["p"] Double
--     , Observables env '[ "y"] Bool)
--   => Model env es Bool
-- coinFlip = do
--   p <- uniform 0 1 #p
--   y <- bernoulli p #y
--   return y


-- coinFlip' :: forall env es. (Observables env '["p"] Double, Observables env '[ "y"] Bool) => Model env es Bool
-- coinFlip' = Model $ do
--   maybe_p  <- call (Ask @env #p)
--   p        <- call (Dist (UniformDist 0 1) maybe_p (Just "p"))
--   maybe_y  <- call (Ask @env #y)
--   y        <- call (Dist (BernoulliDist p) maybe_y (Just "p") )
--   return y


-- main :: IO ()
-- main = do
--   let env = #p := [0.5] <:> #y := [] <:> nil

--   let loop :: Int -> IO [Bool]
--       loop 0 = do
--         return []
--       loop n = do
--         (x, _) <- sampleIO $ runSimulate coinFlip' env
--         xs <- loop (n - 1)
--         return $ x : xs

--   xs <- loop 100

--   print (length $ filter id xs)