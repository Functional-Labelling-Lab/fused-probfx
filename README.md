![SEGP_logo](https://user-images.githubusercontent.com/44177991/212548900-e893d055-e3d0-4e4e-b018-73b65199a594.svg)

## What is fused-probfx?
Fused-probfx is a dsl for probabilistic programming embedded within haskell.
- Allows for multimodal models
- Small and modular - easy to extend
- Models are first-class
- Written using [`fused-effects`](https://github.com/fused-effects/fused-effects)

## Lineage
This library originates from [ProbFX](https://github.com/min-nguyen/prob-fx)

In order to facilitate work on other projects we adapted and extended probFX and rewrote it to use the fused-effects algebraic effects system.
- Moving to using fused-effects rather than a probFX specific effects implementation
- Improving documentation and the provided interfaces
- Improving extensibility

## Usage
### [Examples](examples)

The [examples](examples) directory contains several example programs.

In general, the process is:

1. Define an appropriate model of type `Model env es a`, and (optionally) a corresponding model environment type `env`.

    For example, a logistic regression model that takes a list of `Double`s as inputs and generates a list of `Bool`s, modelling the probability of an event occurring or not:
    ```haskell
    type LogRegrEnv =
    '[  "y" ':= Bool,   -- ^ output
        "m" ':= Double, -- ^ mean
        "b" ':= Double  -- ^ intercept
     ]

    -- | Logistic regression model
    logRegr
    -- Specify the "observable variables" that may later be provided observed values
    :: forall env sig m. (Observable env "y" Bool, Observables env '["m", "b"] Double)
    -- | Model inputs
    => [Double]
    -- | Event occurrences
    -> Model env sig m [Bool]
    logRegr xs = do
      -- Specify model parameter distributions
      {- Annotating with the observable variable #m lets us later provide observed
        values for m. -}
      m     <- normal 0 5 #m
      b     <- normal 0 1 #b
      {- One can use primed variants of distributions which don't require observable
        variables to be provided. This disables being able to later provide
        observed values to that variable. -}
      sigma <- gamma' 1 1
      -- Specify model output distributions
      foldM (\ys x -> do
                    -- probability of event occurring
                    p <- normal' (m * x + b) sigma
                    -- generate as output whether the event occurs
                    y <- bernoulli (sigmoid p) #y
                    return (ys ++ [y])) [] xs
    ```
    The `Observables` constraint says that, for example, `"m"` and `"b"` are observable variables in the model environment `env` that may later be provided a trace of observed values of type `Double`.

    Calling a primitive distribution such as `normal 0 5 #m` lets us later provide observed values for "m" when executing the model.

    Calling a primed variant of primitive distribution such as `gamma' 1 1` will disable observed values from being provided to that distribution.

2. Execute a model under a model environment, using one of the `Inference` library functions.

   Below simulates from a logistic regression model using model parameters `m = 2` and `b = -0.15` but provides no values for `y`: this will result in `m` and `b` being *observed*  but `y` being *sampled*.
    ```haskell
    -- | Simulate from logistic regression
    simulateLogRegr :: Sampler [(Double, Bool)]
    simulateLogRegr = do
      -- First declare the model inputs
      let xs  = map (/50) [(-50) .. 50]
      -- Define a model environment to simulate from, providing observed values for the model parameters
          env :: Env LogRegrEnv
          env = (#y := []) <:> (#m := [2]) <:> (#b := [-0.15]) <:> nil
      -- Call simulate on logistic regression
      (ys, envs) <- SIM.simulate env $ logRegr @LogRegrEnv xs
      return (zip xs ys)
    ```

    We can also do a likelihood weighting.
    ```haskell
    -- | Likelihood-weighting over logistic regression
    inferLwLogRegr :: Sampler [(Double, Double)]
    inferLwLogRegr = do
      -- Get values from simulating log regr
      (xs, ys) <- unzip <$> simulateLogRegr
      let -- Define environment for inference, providing observed values for the model outputs
          env :: Env LogRegrEnv
          env = (#y := ys) <:> (#m := []) <:> (#b := []) <:> nil
      -- Run LW inference for 20000 iterations
      lwTrace :: [(Env LogRegrEnv, Double)] <- LW.lw 20000 env $ logRegr @LogRegrEnv xs
      let -- Get output of LW, extract mu samples, and pair with likelihood-weighting ps
          (env_outs, ps) = unzip lwTrace
          mus = concatMap (get #m) env_outs
      return $ zip mus ps
    ```

    Or perform metropolis hastings inference on the same model by providing values for the model output `y` and hence *observing* (conditioning against) them, but providing none for the model parameters `m` and `b` and hence *sampling* them.
    ```haskell
    -- | Metropolis-Hastings inference over logistic regression
    inferMHLogRegr :: Sampler [(Double, Double)]
    inferMHLogRegr = do
      -- Get values from simulating log regr
      (xs, ys) <- unzip <$> simulateLogRegr
      let -- Define an environment for inference, providing observed values for the model outputs
          env :: Env LogRegrEnv
          env = (#y := ys) <:> (#m := []) <:> (#b := []) <:> nil
      -- Run MH inference for 20000 iterations
      {- The agument ["m", "b"] is optional for indicating interest in learning #m and #b in particular,
        causing other variables to not be resampled (unless necessary) during MH. -}
      mhTrace <- MH.mhRaw 50000 (logRegr xs) env nil (#m <:> #b <:> nil)
      -- Retrieve values sampled for #m and #b during MH
      let m_samples = concatMap (get #m) mhTrace
          b_samples = concatMap (get #b) mhTrace
      return (zip m_samples b_samples)
    ```
    
    One may have noticed by now that *lists* of values are always provided to observable variables in a model environment; each run-time occurrence of that variable will then result in the head value being observed and consumed, and running out of values will default to sampling.

    Running the function `mh` returns a trace of output model environments, from which we can retrieve the trace of sampled model parameters via `get #m` and `get #b`. These represent the posterior distribution over `m` and `b`. (The argument `["m", "b"]` to `mh` is optional for indicating interest in learning `#m` and `#b` in particular).

3. `Sampler` computations can be evaluated with `sampleIO :: Sampler a -> IO a` to produce an `IO` computation.

    ```haskell
    sampleIO simulateLogRegr :: IO [(Double, Bool)]
    ```

## Development
### Project Structure
```bash
.
├── README.md # This readme
├── Setup.hs  
├── benchmarks
│   ├── README.md
│   ├── benchmark.py # Run all benchmarks and tabulate results
│   │
│   │   # fused-probfx, probfx, monad-bays and turing each have a directory
│   └── <benchmark-name> 
│        ├── benchmark-result.csv # benchmark results
│        └── bench.sh             # run the benchmarks
│
├── cabal.project
│
├── examples
│   ├── # ... Example programs
│   │
│   ├── README.md
│   ├── Test       # Tests
│   └── graph.py   # Script for graphing results from example programs
│
├── fused-probfx.cabal
│
└── src
    ├── Control    # Effects
    ├── Data       # Product types
    ├── Inference  # SIM, LW and MH
    ├── Env.hs
    ├── Model.hs
    ├── PrimDist.hs
    ├── Sampler.hs
    └── Trace.hs
```

### [Benchmarking](benchmarks)

### [Testing](examples/Test/)

## Contributions
Contributions are welcome. The functional labelling lab project has ended, so this repository is not actively maintained.

We would like to thank [Min Nguyen](https://github.com/min-nguyen) for building the original [ProbFX](https://github.com/min-nguyen/prob-fx) (without which this project would not be possible) and advice during our lab, as well as our lab supervisor [Dr Nicolas Wu](https://github.com/zenzike) for extensive feedback, direction and for suggesting the project itself.
