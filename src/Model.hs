{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    Model
    -- * Distribution smart constructors
    -- $Smart-Constructors
  , bernoulli
  , bernoulli'
  , beta
  , beta'
  , binomial
  , binomial'
  , categorical
  , categorical'
  , cauchy
  , cauchy'
  , halfCauchy
  , halfCauchy'
  , deterministic
  , deterministic'
  , dirichlet
  , dirichlet'
  , discrete
  , discrete'
  , gamma
  , gamma'
  , normal
  , normal'
  , halfNormal
  , halfNormal'
  , poisson
  , poisson'
  , uniform
  , uniform'
  )
  where

import           Control.Algebra           (Has, send)
import           Control.Carrier.Draw      (DrawC, runDraw)
import           Control.Carrier.ObsReader (ObsReaderC, runObsReader)
import           Control.Effect.Draw       (Draw (Draw), draw)
import           Control.Effect.Lift       (Lift (..))
import           Control.Effect.ObsReader  (ObsReader, ask)
import           Control.Effect.Sum        ((:+:))
import           Control.Monad             (ap)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Env                       (Env, ObsVar, Observable, varToStr)
import qualified OpenSum
import           PrimDist                  (PrimDist (..), PrimVal)

type Model env = ObsReader env :+: Draw

{- $Smart-Constructors

    Smart constructors for calling primitive distribution operations inside models,
    where each distribution comes with a primed and an unprimed variant.

    An unprimed distribution takes the standard distribution parameters as well as
    an observable variable. This lets one later provide observed values for that
    variable to be conditioned against:

    @
    exampleModel :: Observable env "b" Bool => Model m Bool
    exampleModel = bernoulli 0.5 #b
    @

    A primed distribution takes no observable variable and so cannot be conditioned against;
    this will always representing sampling from that distribution:

    @
    exampleModel' :: Model m Bool
    exampleModel' = bernoulli' 0.5
    @
-}

envDraw :: forall env sig m a x. (Observable env x a, Has (ObsReader env :+: Draw) sig m)
  => PrimDist a
  -> ObsVar x
  -> m a
envDraw primDist field = do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  draw primDist maybe_y tag

deterministic :: forall env sig m a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a, Has (ObsReader env :+: Draw) sig m)
  => a
  -> ObsVar x
  -> m a
deterministic x = envDraw @env (DeterministicDist x)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal, Has (ObsReader env :+: Draw) sig m)
  => a -- ^ value to be deterministically generated
  -> m a
deterministic' x =
  draw (DeterministicDist x) Nothing Nothing

dirichlet :: forall env sig m x. (Observable env x [Double], Has (ObsReader env :+: Draw) sig m)
  => [Double]
  -> ObsVar x
  -> m [Double]
dirichlet xs = envDraw @env (DirichletDist xs)

dirichlet' :: (Has (ObsReader env :+: Draw) sig m)
  => [Double] -- ^ concentration parameters
  -> m [Double]
dirichlet' xs = draw (DirichletDist xs) Nothing Nothing

discrete :: forall env sig m x. (Observable env x Int, Has (ObsReader env :+: Draw) sig m)
  => [Double]
  -> ObsVar x
  -> m Int
discrete ps = envDraw @env (DiscreteDist ps)

discrete' :: (Has (ObsReader env :+: Draw) sig m)
  => [Double]         -- ^ list of @n@ probabilities
  -> m Int -- ^ integer index from @0@ to @n - 1@
discrete' ps = draw (DiscreteDist ps) Nothing Nothing

categorical :: forall env sig m x a. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a, Has (ObsReader env :+: Draw) sig m)
  => [(a, Double)]
  -> ObsVar x
  -> m a
categorical xs = envDraw @env (CategoricalDist xs)

categorical' :: (Eq a, Show a, OpenSum.Member a PrimVal, Has (ObsReader env :+: Draw) sig m)
  => [(a, Double)] -- ^ primitive values and their probabilities
  -> m a
categorical' xs = draw (CategoricalDist xs) Nothing Nothing

normal :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
normal mu sigma = envDraw @env (NormalDist mu sigma)

normal' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ mean
  -> Double -- ^ standard deviation
  -> m Double
normal' mu sigma = draw (NormalDist mu sigma) Nothing Nothing

halfNormal :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> ObsVar x
  -> m Double
halfNormal sigma = envDraw @env (HalfNormalDist sigma)

halfNormal' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ standard deviation
  -> m Double
halfNormal' sigma = draw (HalfNormalDist sigma) Nothing Nothing

cauchy :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
cauchy mu sigma = envDraw @env (CauchyDist mu sigma)

cauchy' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ location
  -> Double -- ^ scale
  -> m Double
cauchy' mu sigma = draw (CauchyDist mu sigma) Nothing Nothing

halfCauchy :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> ObsVar x
  -> m Double
halfCauchy sigma = envDraw @env (HalfCauchyDist sigma)

halfCauchy' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ scale
  -> m Double
halfCauchy' sigma = draw (HalfCauchyDist sigma) Nothing Nothing

bernoulli :: forall env sig m x. (Observable env x Bool, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> ObsVar x
  -> m Bool
bernoulli p = envDraw @env (BernoulliDist p)

bernoulli' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ probability of @True@
  -> m Bool
bernoulli' p = draw (BernoulliDist p) Nothing Nothing

beta :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
beta α β = envDraw @env (BetaDist α β)

beta' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ shape 1 (α)
  -> Double -- ^ shape 2 (β)
  -> m Double
beta' α β = draw (BetaDist α β) Nothing Nothing

binomial :: forall env sig m x. (Observable env x Int, Has (ObsReader env :+: Draw) sig m)
  => Int
  -> Double
  -> ObsVar x
  -> m Int
binomial n p = envDraw @env (BinomialDist n p)

binomial' :: (Has (ObsReader env :+: Draw) sig m)
  => Int              -- ^ number of trials
  -> Double           -- ^ probability of successful trial
  -> m Int -- ^ number of successful trials
binomial' n p = draw (BinomialDist n p) Nothing Nothing

gamma :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
gamma k θ = envDraw @env (GammaDist k θ)

gamma' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ shape (k)
  -> Double -- ^ scale (θ)
  -> m Double
gamma' k θ = draw (GammaDist k θ) Nothing Nothing

uniform :: forall env sig m x. (Observable env x Double, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
uniform min max = envDraw @env (UniformDist min max)

uniform' :: (Has (ObsReader env :+: Draw) sig m)
  => Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> m Double
uniform' min max = draw (UniformDist min max) Nothing Nothing

poisson :: forall env sig m x. (Observable env x Int, Has (ObsReader env :+: Draw) sig m)
  => Double
  -> ObsVar x
  -> m Int
poisson λ = envDraw @env (PoissonDist λ)

poisson' :: (Has (ObsReader env :+: Draw) sig m)
  => Double           -- ^ rate (λ)
  -> m Int -- ^ number of events
poisson' λ = draw (PoissonDist λ) Nothing Nothing
