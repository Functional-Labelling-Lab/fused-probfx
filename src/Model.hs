{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

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
import           Control.Carrier.Dist      (DistC, runDist)
import           Control.Carrier.ObsReader (ObsReaderC, runObsReader)
import           Control.Effect.Dist       (Dist (Dist), dist)
import           Control.Effect.Lift       (Lift (..))
import           Control.Effect.ObsReader  (ObsReader, ask)
import           Control.Effect.Sum        ((:+:), Member (..))
import           Control.Monad             (ap)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Env                       (Assign (..), Env, ObsVar,
                                            Observable, varToStr)
import qualified OpenSum
import           PrimDist                  (PrimDist (..), PrimVal)
import GHC.TypeLits (Symbol)
import Control.Effect.EffUnion (EffUnion)

type Model env = EffUnion ObsReader env :+: Dist

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

envDist :: forall x a sig m. (Has (ObsReader (x := a) :+: Dist) sig m)
  => PrimDist a
  -> ObsVar x
  -> m a
envDist primDist field = do
  let tag = Just $ varToStr field
  maybe_y <- ask @x
  dist primDist maybe_y tag

deterministic :: (Eq a, Show a, OpenSum.Member a PrimVal, Has (ObsReader (x := a) :+: Dist) sig m)
  => a
  -> ObsVar x
  -> m a
deterministic x = envDist (DeterministicDist x)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal, Has Dist sig m)
  => a -- ^ value to be deterministically generated
  -> m a
deterministic' x =
  dist (DeterministicDist x) Nothing Nothing

dirichlet :: Has (ObsReader (x := [Double]) :+: Dist) sig m
  => [Double]
  -> ObsVar x
  -> m [Double]
dirichlet xs = envDist (DirichletDist xs)

dirichlet' :: (Has Dist sig m)
  => [Double] -- ^ concentration parameters
  -> m [Double]
dirichlet' xs = dist (DirichletDist xs) Nothing Nothing

discrete :: (Has (ObsReader (x := Int) :+: Dist) sig m)
  => [Double]
  -> ObsVar x
  -> m Int
discrete ps = envDist (DiscreteDist ps)

discrete' :: (Has Dist sig m)
  => [Double]         -- ^ list of @n@ probabilities
  -> m Int -- ^ integer index from @0@ to @n - 1@
discrete' ps = dist (DiscreteDist ps) Nothing Nothing

categorical :: (Eq a, Show a, OpenSum.Member a PrimVal, Has (ObsReader (x := a) :+: Dist) sig m)
  => [(a, Double)]
  -> ObsVar x
  -> m a
categorical xs = envDist (CategoricalDist xs)

categorical' :: (Eq a, Show a, OpenSum.Member a PrimVal, Has Dist sig m)
  => [(a, Double)] -- ^ primitive values and their probabilities
  -> m a
categorical' xs = dist (CategoricalDist xs) Nothing Nothing

normal :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
normal mu sigma = envDist (NormalDist mu sigma)

normal' :: (Has Dist sig m)
  => Double -- ^ mean
  -> Double -- ^ standard deviation
  -> m Double
normal' mu sigma = dist (NormalDist mu sigma) Nothing Nothing

halfNormal :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> ObsVar x
  -> m Double
halfNormal sigma = envDist (HalfNormalDist sigma)

halfNormal' :: (Has Dist sig m)
  => Double -- ^ standard deviation
  -> m Double
halfNormal' sigma = dist (HalfNormalDist sigma) Nothing Nothing

cauchy :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
cauchy mu sigma = envDist (CauchyDist mu sigma)

cauchy' :: (Has Dist sig m)
  => Double -- ^ location
  -> Double -- ^ scale
  -> m Double
cauchy' mu sigma = dist (CauchyDist mu sigma) Nothing Nothing

halfCauchy :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> ObsVar x
  -> m Double
halfCauchy sigma = envDist (HalfCauchyDist sigma)

halfCauchy' :: (Has Dist sig m)
  => Double -- ^ scale
  -> m Double
halfCauchy' sigma = dist (HalfCauchyDist sigma) Nothing Nothing

bernoulli :: (Has (ObsReader (x := Bool) :+: Dist) sig m)
  => Double
  -> ObsVar x
  -> m Bool
bernoulli p = envDist (BernoulliDist p)

bernoulli' :: (Has Dist sig m)
  => Double -- ^ probability of @True@
  -> m Bool
bernoulli' p = dist (BernoulliDist p) Nothing Nothing

beta :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
beta α β = envDist (BetaDist α β)

beta' :: (Has Dist sig m)
  => Double -- ^ shape 1 (α)
  -> Double -- ^ shape 2 (β)
  -> m Double
beta' α β = dist (BetaDist α β) Nothing Nothing

binomial :: (Has (ObsReader (x := Int) :+: Dist) sig m)
  => Int
  -> Double
  -> ObsVar x
  -> m Int
binomial n p = envDist (BinomialDist n p)

binomial' :: (Has Dist sig m)
  => Int              -- ^ number of trials
  -> Double           -- ^ probability of successful trial
  -> m Int -- ^ number of successful trials
binomial' n p = dist (BinomialDist n p) Nothing Nothing

gamma :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
gamma k θ = envDist (GammaDist k θ)

gamma' :: (Has Dist sig m)
  => Double -- ^ shape (k)
  -> Double -- ^ scale (θ)
  -> m Double
gamma' k θ = dist (GammaDist k θ) Nothing Nothing

uniform :: (Has (ObsReader (x := Double) :+: Dist) sig m)
  => Double
  -> Double
  -> ObsVar x
  -> m Double
uniform min max = envDist (UniformDist min max)

uniform' :: (Has Dist sig m)
  => Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> m Double
uniform' min max = dist (UniformDist min max) Nothing Nothing

poisson :: (Has (ObsReader (x := Int) :+: Dist) sig m)
  => Double
  -> ObsVar x
  -> m Int
poisson λ = envDist (PoissonDist λ)

poisson' :: (Has Dist sig m)
  => Double           -- ^ rate (λ)
  -> m Int -- ^ number of events
poisson' λ = dist (PoissonDist λ) Nothing Nothing
