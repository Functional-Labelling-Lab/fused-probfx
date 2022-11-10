{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- | An algebraic effect embedding of probabilistic models.
-}

module Control.Model (
    Model(..)
  , handleCore
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
import           Control.Effect.Dist       (Dist (Dist))
import           Control.Effect.Lift       (Lift (..))
import           Control.Effect.Observe    (Observe)
import           Control.Effect.ObsReader  (ObsReader, ask)
import           Control.Effect.Sample     (Sample)
import           Control.Effect.Sum        (type (:+:))
import           Control.Monad             (ap)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Env                       (Env, ObsVar, Observable, varToStr)
import qualified OpenSum
import           PrimDist                  (PrimDist (..), PrimVal)
import           Prog                      (Member, Prog, call)

{- | Models are parameterised by:

    1) a model environment @env@ containing random variables that can be provided observed values

    2) an effect signature @es@ of the possible effects a model can invoke

    3) an output type @a@ of values that the model generates.

    A model initially consists of (at least) two effects: @Dist@ for calling primitive distributions
    and @ObsReader env@ for reading from @env@.
-}
newtype Model env sig m a =
  Model { runModel :: (Has Dist sig m, Has (ObsReader env) sig m) => m a }
  deriving Functor

instance Functor m => Applicative (Model env sig m) where
  pure x = Model $ pure x
  (<*>) = ap

instance Functor m => Monad (Model env sig m) where
  return = pure
  Model f >>= x = Model $ do
    f' <- f
    runModel $ x f'

{- | The initial handler for models, specialising a model under a certain
environment to produce a probabilistic program consisting of @Sample@ and @Observe@ operations. -}
handleCore :: (Has Observe sig m, Has Sample sig m)
           => Model env (ObsReader env :+: Dist :+: sig) (ObsReaderC env (DistC m)) a
           -> Env env
           -> m a
handleCore m env = runDist $ runObsReader env $ runModel m

{- $Smart-Constructors

    Smart constructors for calling primitive distribution operations inside models,
    where each distribution comes with a primed and an unprimed variant.

    An unprimed distribution takes the standard distribution parameters as well as
    an observable variable. This lets one later provide observed values for that
    variable to be conditioned against:

    @
    exampleModel :: Observable env "b" Bool => Model env sig m Bool
    exampleModel = bernoulli 0.5 #b
    @

    A primed distribution takes no observable variable and so cannot be conditioned against;
    this will always representing sampling from that distribution:

    @
    exampleModel' :: Model env sig m Bool
    exampleModel' = bernoulli' 0.5
    @
-}

deterministic :: forall env sig m a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a) => a
  -> ObsVar x
  -> Model env sig m a
deterministic x field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (DeterministicDist x) maybe_y tag)

deterministic' :: (Eq a, Show a, OpenSum.Member a PrimVal) =>
     a -- ^ value to be deterministically generated
  -> Model env sig m a
deterministic' x = Model $ do
  send (Dist (DeterministicDist x) Nothing Nothing)

dirichlet :: forall env sig m x. Observable env x [Double] =>
     [Double]
  -> ObsVar x
  -> Model env sig m [Double]
dirichlet xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (DirichletDist xs) maybe_y tag)

dirichlet' ::
     [Double] -- ^ concentration parameters
  -> Model env sig m [Double]
dirichlet' xs = Model $ do
  send (Dist (DirichletDist xs) Nothing Nothing)

discrete :: forall env sig m x. Observable env x Int =>
     [Double]
  -> ObsVar x
  -> Model env sig m Int
discrete ps field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (DiscreteDist ps) maybe_y tag)

discrete' ::
     [Double]         -- ^ list of @n@ probabilities
  -> Model env sig m Int -- ^ integer index from @0@ to @n - 1@
discrete' ps = Model $ do
  send (Dist (DiscreteDist ps) Nothing Nothing)

categorical :: forall env sig m a x. (Eq a, Show a, OpenSum.Member a PrimVal, Observable env x a) =>
     [(a, Double)]
  -> ObsVar x
  -> Model env sig m a
categorical xs field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (CategoricalDist xs) maybe_y tag)

categorical' :: (Eq a, Show a, OpenSum.Member a PrimVal) =>
     [(a, Double)] -- ^ primitive values and their probabilities
  -> Model env sig m a
categorical' xs = Model $ do
  send (Dist (CategoricalDist xs) Nothing Nothing)

normal :: forall env sig m x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
normal mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (NormalDist mu sigma) maybe_y tag)

normal' ::
     Double -- ^ mean
  -> Double -- ^ standard deviation
  -> Model env sig m Double
normal' mu sigma = Model $ do
  send (Dist (NormalDist mu sigma) Nothing Nothing)

halfNormal :: forall env sig m x. Observable env x Double =>
     Double
  -> ObsVar x
  -> Model env sig m Double
halfNormal sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (HalfNormalDist sigma) maybe_y tag)

halfNormal' ::
     Double -- ^ standard deviation
  -> Model env sig m Double
halfNormal' sigma = Model $ do
  send (Dist (HalfNormalDist sigma) Nothing Nothing)

cauchy :: forall env sig m x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
cauchy mu sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (CauchyDist mu sigma) maybe_y tag)

cauchy' ::
     Double -- ^ location
  -> Double -- ^ scale
  -> Model env sig m Double
cauchy' mu sigma = Model $ do
  send (Dist (CauchyDist mu sigma) Nothing Nothing)

halfCauchy :: forall env sig m x. Observable env x Double =>
     Double
  -> ObsVar x
  -> Model env sig m Double
halfCauchy sigma field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (HalfCauchyDist sigma) maybe_y tag)

halfCauchy' ::
     Double -- ^ scale
  -> Model env sig m Double
halfCauchy' sigma = Model $ do
  send (Dist (HalfCauchyDist sigma) Nothing Nothing)

bernoulli :: forall env sig m x. Observable env x Bool =>
     Double
  -> ObsVar x
  -> Model env sig m Bool
bernoulli p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (BernoulliDist p) maybe_y tag)

bernoulli' ::
     Double -- ^ probability of @True@
  -> Model env sig m Bool
bernoulli' p = Model $ do
  send (Dist (BernoulliDist p) Nothing Nothing)

beta :: forall env sig m x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
beta α β field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (BetaDist α β) maybe_y tag)

beta' ::
     Double -- ^ shape 1 (α)
  -> Double -- ^ shape 2 (β)
  -> Model env sig m Double
beta' α β = Model $ do
  send (Dist (BetaDist α β) Nothing Nothing)

binomial :: forall env sig m x. Observable env x Int =>
     Int
  -> Double
  -> ObsVar x
  -> Model env sig m Int
binomial n p field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (BinomialDist n p) maybe_y tag)

binomial' ::
     Int              -- ^ number of trials
  -> Double           -- ^ probability of successful trial
  -> Model env sig m Int -- ^ number of successful trials
binomial' n p = Model $ do
  send (Dist (BinomialDist n p) Nothing Nothing)

gamma :: forall env sig m x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
gamma k θ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (GammaDist k θ) maybe_y tag)

gamma' ::
     Double -- ^ shape (k)
  -> Double -- ^ scale (θ)
  -> Model env sig m Double
gamma' k θ = Model $ do
  send (Dist (GammaDist k θ) Nothing Nothing)

uniform :: forall env sig m x. Observable env x Double =>
     Double
  -> Double
  -> ObsVar x
  -> Model env sig m Double
uniform min max field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (UniformDist min max) maybe_y tag)

uniform' ::
     Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> Model env sig m Double
uniform' min max = Model $ do
  send (Dist (UniformDist min max) Nothing Nothing)

poisson :: forall env sig m x. Observable env x Int =>
     Double
  -> ObsVar x
  -> Model env sig m Int
poisson λ field = Model $ do
  let tag = Just $ varToStr field
  maybe_y <- ask @env field
  send (Dist (PoissonDist λ) maybe_y tag)

poisson' ::
     Double           -- ^ rate (λ)
  -> Model env sig m Int -- ^ number of events
poisson' λ = Model $ do
  send (Dist (PoissonDist λ) Nothing Nothing)

