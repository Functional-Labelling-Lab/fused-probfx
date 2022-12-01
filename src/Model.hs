{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Model (
    Model(..)
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

import           Control.Algebra           (Algebra, Has, send)
import           Control.Carrier.Dist      (DistC, runDist)
import           Control.Carrier.ObsReader (ObsReaderC, runObsReader)
import           Control.Effect.Dist       (Dist (Dist), dist)
import           Control.Effect.Labelled   (Algebra (alg))
import           Control.Effect.Lift       (Lift (..))
import           Control.Effect.ObsReader  (ObsReader, ask)
import           Control.Effect.Sum        (Member, Members, (:+:))
import           Control.Monad             (ap)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Env                       (Env, ObsVar, Observable, varToStr)
import qualified OpenSum
import           PrimDist                  (PrimDist (..), PrimVal)

newtype Model env sig m a = Model {runModel :: (Member (ObsReader env) sig, Member Dist sig, Algebra sig m) => m a}

instance Functor (Model env sig m) where
  fmap f (Model x) = Model $ fmap f x

instance Applicative (Model env sig m) where
  pure x = Model $ pure x
  (Model f) <*> (Model x) = Model $ f <*> x

instance Monad (Model env sig m) where
  (Model m) >>= f = Model $ m >>= (\m -> let (Model m') = f m in m')

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

envDist :: forall env x a sig m. Observable env x a
  => PrimDist a
  -> ObsVar x
  -> Model env sig m a
envDist primDist field = do
  let tag = Just $ varToStr field
  maybe_y <- Model $ ask @env field
  Model $ dist primDist maybe_y tag

deterministic :: (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a)
  => a
  -> ObsVar x
  -> Model env sig m a
deterministic x = envDist (DeterministicDist x)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal)
  => a -- ^ value to be deterministically generated
  -> Model env sig m a
deterministic' x =
  Model $ dist (DeterministicDist x) Nothing Nothing

dirichlet :: Observable env x [Double]
  => [Double]
  -> ObsVar x
  -> Model env sig m [Double]
dirichlet xs = envDist (DirichletDist xs)

dirichlet' ::
     [Double] -- ^ concentration parameters
  -> Model env sig m [Double]
dirichlet' xs = Model $ dist (DirichletDist xs) Nothing Nothing

discrete :: Observable env x Int
  => [Double]
  -> ObsVar x
  -> Model env sig m Int
discrete ps = envDist (DiscreteDist ps)

discrete' ::
     [Double]         -- ^ list of @n@ probabilities
  -> Model env sig m Int -- ^ integer index from @0@ to @n - 1@
discrete' ps = Model $ dist (DiscreteDist ps) Nothing Nothing

categorical :: (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a)
  => [(a, Double)]
  -> ObsVar x
  -> Model env sig m a
categorical xs = envDist (CategoricalDist xs)

categorical' :: (Eq a, Show a, OpenSum.Member a PrimVal)
  => [(a, Double)] -- ^ primitive values and their probabilities
  -> Model env sig m a
categorical' xs = Model $ dist (CategoricalDist xs) Nothing Nothing

normal :: Observable env x Double
  => Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
normal mu sigma = envDist (NormalDist mu sigma)

normal' ::
     Double -- ^ mean
  -> Double -- ^ standard deviation
  -> Model env sig m Double
normal' mu sigma = Model $ dist (NormalDist mu sigma) Nothing Nothing

halfNormal :: Observable env x Double
  => Double
  -> ObsVar x
  -> Model env sig m Double
halfNormal sigma = envDist (HalfNormalDist sigma)

halfNormal' ::
     Double -- ^ standard deviation
  -> Model env sig m Double
halfNormal' sigma = Model $ dist (HalfNormalDist sigma) Nothing Nothing

cauchy :: Observable env x Double
  => Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
cauchy mu sigma = envDist (CauchyDist mu sigma)

cauchy' ::
     Double -- ^ location
  -> Double -- ^ scale
  -> Model env sig m Double
cauchy' mu sigma = Model $ dist (CauchyDist mu sigma) Nothing Nothing

halfCauchy :: Observable env x Double
  => Double
  -> ObsVar x
  -> Model env sig m Double
halfCauchy sigma = envDist (HalfCauchyDist sigma)

halfCauchy' ::
     Double -- ^ scale
  -> Model env sig m Double
halfCauchy' sigma = Model $ dist (HalfCauchyDist sigma) Nothing Nothing

bernoulli :: Observable env x Bool
  => Double
  -> ObsVar x
  -> Model env sig m Bool
bernoulli p = envDist (BernoulliDist p)

bernoulli' ::
     Double -- ^ probability of @True@
  -> Model env sig m Bool
bernoulli' p = Model $ dist (BernoulliDist p) Nothing Nothing

beta :: Observable env x Double
  => Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
beta α β = envDist (BetaDist α β)

beta' ::
     Double -- ^ shape 1 (α)
  -> Double -- ^ shape 2 (β)
  -> Model env sig m Double
beta' α β = Model $ dist (BetaDist α β) Nothing Nothing

binomial :: Observable env x Int
  => Int
  -> Double
  -> ObsVar x
  -> Model env sig m Int
binomial n p = envDist (BinomialDist n p)

binomial' ::
     Int              -- ^ number of trials
  -> Double           -- ^ probability of successful trial
  -> Model env sig m Int -- ^ number of successful trials
binomial' n p = Model $ dist (BinomialDist n p) Nothing Nothing

gamma :: Observable env x Double
  => Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
gamma k θ = envDist (GammaDist k θ)

gamma' ::
     Double -- ^ shape (k)
  -> Double -- ^ scale (θ)
  -> Model env sig m Double
gamma' k θ = Model $ dist (GammaDist k θ) Nothing Nothing

uniform :: Observable env x Double
  => Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
uniform min max = envDist (UniformDist min max)

uniform' ::
     Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> Model env sig m Double
uniform' min max = Model $ dist (UniformDist min max) Nothing Nothing

poisson :: Observable env x Int
  => Double
  -> ObsVar x
  -> Model env sig m Int
poisson λ = envDist (PoissonDist λ)

poisson' ::
     Double           -- ^ rate (λ)
  -> Model env sig m Int -- ^ number of events
poisson' λ = Model $ dist (PoissonDist λ) Nothing Nothing
