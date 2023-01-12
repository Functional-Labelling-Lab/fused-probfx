{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
import Sampler (Sampler, sampleIO, liftS)
import Env (Observables, Assign (..), (<:>), nil, Env, get)
import Model (Model (..), normal, uniform, normal')
import Inference.SIM (simulate)
import Inference.MH (Transitions, mh)
import Control.Algebra (Has)
import Control.Effect.Lift (Lift)
import Control.Carrier.Lift (sendM)

type ModelEnv = '["x" := Double, "y" := Double, "z" := Double]
type ModelTrans = '["x" := Double, "y" := Double]

-- Basic Example
model :: Observables env ["x", "y", "z"] Double => Double -> Double -> Model env sig m Double
model a b = do
    x <- normal 0 1 #x
    y <- uniform (-1) 1 #y
    normal (a * x + b * y) 0.1 #z

simExample :: IO (Double, Double)
simExample = sampleIO $ do
    let env :: Env ModelEnv
        env = (#x := [2]) <:> (#y := []) <:> (#z := []) <:> nil
    (_, env_out) <- simulate env (model 1 1)
    return (head $ get #y env_out, head $ get #z env_out)

-- main = simExample >>= print

-- Example with Metropolis Hastings
modelExp :: Observables env ["x", "y", "z"] Double => Model env sig m Double
modelExp = do
    x <- normal 0 1 #x
    y <- uniform (-1) 1 #y
    normal (exp y - x) 0.001 #z

transModel :: Double -> Double -> Model env sig m Double
transModel std mean = normal' mean std

mhExample :: IO (Double, Double, Double)
mhExample = sampleIO $ do
    let env :: Env ModelEnv
        env = (#x := []) <:> (#y := []) <:> (#z := [0]) <:> nil

    let trans :: Transitions ModelTrans
        trans = (#x := transModel 20) <:> (#y := transModel 1) <:> nil

    env_out <- mh 2 modelExp env trans (#x <:> nil)
    let x = head $ get #x $ head $ env_out
    let y = head $ get #y $ head $ env_out
    return (x, y, exp y - x)

-- main = mhExample >>= print

-- Example with effects
modelEff :: (Has (Lift Sampler) sig m, Observables env ["x", "y", "z"] Double) => Model env sig m ()
modelEff = do
    x <- normal 0 1 #x
    y <- uniform (-1) 1 #y
    Model $ sendM $ liftS $ print x
    Model $ sendM $ liftS $ print y
    return ()

effExample :: IO ()
effExample = sampleIO $ do
    let env :: Env ModelEnv
        env = (#x := []) <:> (#y := [3]) <:> (#z := []) <:> nil
    simulate env modelEff
    return ()

-- main = effExample
