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
import Inference.MH (mh)
import Effects.Lift (Lift, lift, handleLift)
import Prog (Member, call, Prog)

type ModelEnv = '["x" := Double, "y" := Double, "z" := Double]
type ModelTrans = '["x" := Double, "y" := Double]

model :: Observables env ["x", "y", "z"] Double => Double -> Double -> Model env sig Double
model a b = do
    x <- normal 0 1 #x
    y <- uniform (-1) 1 #y
    normal (a * x + b * y) 1 #z

simExample :: IO (Double, Double)
simExample = sampleIO $ do
    let env :: Env ModelEnv
        env = (#x := [2]) <:> (#y := []) <:> (#z := []) <:> nil
    (_, env_out) <- simulate (const $ model 1 1) env ()
    return (head $ get #y env_out, head $ get #z env_out)

modelExp :: Observables env ["x", "y", "z"] Double => Model env sig Double
modelExp = do
    x <- normal 0 1 #x
    y <- uniform (-1) 1 #y
    normal (exp y - x) 0.001 #z

transModel :: Double -> Double -> Model env sig Double
transModel std mean = normal' mean std

advancedExample :: IO ([Double], [Double])
advancedExample = sampleIO $ do
    let env :: Env ModelEnv
        env = (#x := []) <:> (#y := []) <:> (#z := [0]) <:> nil

    env_out <- mh 2 (const modelExp) ((), env) ["x"]
    return (map (head . get #x) env_out, map (head . get #y) env_out)

modelEff :: Observables env ["x", "y", "z"] Double => () -> Model env es ()
modelEff _ = do
    x <- normal 0 1 #x
    y <- uniform (-1) 1 #y
    return ()

effExample :: IO ()
effExample = sampleIO $ do
    let env :: Env ModelEnv
        env = (#x := []) <:> (#y := [3]) <:> (#z := []) <:> nil
    simulate modelEff env ()
    return ()