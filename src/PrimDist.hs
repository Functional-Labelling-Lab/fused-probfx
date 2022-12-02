{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{- | A GADT encoding of (a selection of) primitive distributions
    along with their corresponding sampling and density functions.
-}

module PrimDist (
  -- * Primitive distribution
    PrimDist(..)
  , PrimVal
  , IsPrimVal(..)
  , pattern PrimDistPrf
  , ErasedPrimDist(..)
  -- * Disting sample
  , dist
  -- * Density
  , prob
  , logProb
  -- ** Address
  -- $Address
  , Tag
  , Addr
  ) where

import           Data.Kind                               (Constraint)
import qualified Data.Map                                as Map
import qualified Data.Vector                             as V
import qualified Data.Vector.Unboxed                     as UV
import           Numeric.Log                             (Log (..))
import qualified OpenSum
import Control.Effect.Sampler
    ( Sampler,
      createSampler,
      sampleCauchy,
      sampleNormal,
      sampleUniform,
      sampleDiscreteUniform,
      sampleGamma,
      sampleBeta,
      sampleBernoulli,
      sampleBinomial,
      sampleCategorical,
      sampleDiscrete,
      samplePoisson,
      sampleDirichlet ) 
import           Statistics.Distribution                 (ContDistr (density),
                                                          DiscreteDistr (probability))
import           Statistics.Distribution.Beta            (betaDistr)
import           Statistics.Distribution.Binomial        (binomial)
import           Statistics.Distribution.CauchyLorentz   (cauchyDistribution)
import           Statistics.Distribution.Dirichlet       (dirichletDensity,
                                                          dirichletDistribution)
import           Statistics.Distribution.DiscreteUniform (discreteUniformAB)
import           Statistics.Distribution.Gamma           (gammaDistr)
import           Statistics.Distribution.Normal          (normalDistr)
import           Statistics.Distribution.Poisson         (poisson)
import           Statistics.Distribution.Uniform         (uniformDistr)
import qualified System.Random.MWC.Distributions         as MWC
import Control.Algebra (Has)

-- | Primitive distribution
data PrimDist a where
  BernoulliDist
    :: Double           -- ^ probability of @True@
    -> PrimDist Bool
  BetaDist
    :: Double           -- ^ shape α
    -> Double           -- ^ shape β
    -> PrimDist Double
  BinomialDist
    :: Int              -- ^ number of trials
    -> Double           -- ^ probability of successful trial
    -> PrimDist Int
  CategoricalDist
    :: (Eq a, Show a, OpenSum.Member a PrimVal)
    => [(a, Double)]    -- ^ values and associated probabilities
    -> PrimDist a
  CauchyDist
    :: Double           -- ^ location
    -> Double           -- ^ scale
    -> PrimDist Double
  HalfCauchyDist
    :: Double           -- ^ scale
    -> PrimDist Double
  DeterministicDist
    :: (Eq a, Show a, OpenSum.Member a PrimVal)
    => a                -- ^ value of probability @1@
    -> PrimDist a
  DirichletDist
    :: [Double]         -- ^ concentrations
    -> PrimDist [Double]
  DiscreteDist
    :: [Double]         -- ^ list of @n@ probabilities
    -> PrimDist Int     -- ^ an index from @0@ to @n - 1@
  DiscrUniformDist
    :: Int              -- ^ lower-bound @a@
    -> Int              -- ^ upper-bound @b@
    -> PrimDist Int
  GammaDist
    :: Double           -- ^ shape k
    -> Double           -- ^ scale θ
    -> PrimDist Double
  NormalDist
    :: Double           -- ^ mean
    -> Double           -- ^ standard deviation
    -> PrimDist Double
  HalfNormalDist
    :: Double           -- ^ standard deviation
    -> PrimDist Double
  PoissonDist
    :: Double           -- ^ rate λ
    -> PrimDist Int
  UniformDist
    :: Double           -- ^ lower-bound @a@
    -> Double           -- ^ upper-bound @b@
    -> PrimDist Double

instance Eq (PrimDist a) where
  (==) (NormalDist m s) (NormalDist m' s') = m == m' && s == s'
  (==) (CauchyDist m s) (CauchyDist m' s') = m == m' && s == s'
  (==) (HalfCauchyDist s) (HalfCauchyDist s') = s == s'
  (==) (HalfNormalDist s) (HalfNormalDist s') = s == s'
  (==) (BernoulliDist p) (BernoulliDist p') = p == p'
  (==) (BinomialDist n p) (BinomialDist n' p') = n == n' && p == p'
  (==) (DiscreteDist ps) (DiscreteDist ps') = ps == ps'
  (==) (BetaDist a b) (BetaDist a' b') = a == a' && b == b'
  (==) (GammaDist a b) (GammaDist a' b') = a == a' && b == b'
  (==) (UniformDist a b) (UniformDist a' b') = a == a' && b == b'
  (==) (DiscrUniformDist min max) (DiscrUniformDist min' max') = min == min' && max == max'
  (==) (PoissonDist l) (PoissonDist l') = l == l'
  (==) (CategoricalDist xs) (CategoricalDist xs') = xs == xs'
  (==) (DirichletDist xs) (DirichletDist xs')  = xs == xs'
  (==) (DeterministicDist x) (DeterministicDist x') = x == x'
  (==) _ _ = False

instance Show a => Show (PrimDist a) where
  show (CauchyDist mu sigma) =
   "CauchyDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ ")"
  show (HalfCauchyDist sigma) =
   "HalfCauchyDist(" ++ show sigma ++ ", " ++ ")"
  show (NormalDist mu sigma) =
   "NormalDist(" ++ show mu ++ ", " ++ show sigma ++ ", " ++ ")"
  show (HalfNormalDist sigma) =
   "HalfNormalDist(" ++ show sigma ++ ", " ++ ")"
  show (BernoulliDist p) =
   "BernoulliDist(" ++ show p ++ ", " ++ ")"
  show (BinomialDist n p) =
   "BinomialDist(" ++ show n ++ ", " ++ show p ++ ", " ++  ")"
  show (DiscreteDist ps) =
   "DiscreteDist(" ++ show ps ++ ", " ++ ")"
  show (BetaDist a b) =
   "BetaDist(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (GammaDist a b) =
   "GammaDist(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (UniformDist a b) =
   "UniformDist(" ++ show a ++ ", " ++ show b ++ "," ++ ")"
  show (DiscrUniformDist min max) =
   "DiscrUniformDist(" ++ show min ++ ", " ++ show max ++ ", " ++ ")"
  show (PoissonDist l) =
   "PoissonDist(" ++ show l ++ ", " ++ ")"
  show (CategoricalDist xs) =
   "CategoricalDist(" ++ show xs ++ ", " ++ ")"
  show (DirichletDist xs) =
   "DirichletDist(" ++ show xs ++ ", " ++ ")"
  show (DeterministicDist x) =
   "DeterministicDist(" ++ show x ++ ", " ++ ")"

-- | An ad-hoc specification of primitive value types, for constraining the outputs of distributions
type PrimVal = '[Int, Double, [Double], Bool, String]

-- | Proof that @x@ is a primitive value
data IsPrimVal x where
  IsPrimVal :: (Show x, OpenSum.Member x PrimVal) => IsPrimVal x

-- | For pattern-matching on an arbitrary @PrimDist@ with proof that it generates a primitive value
pattern PrimDistPrf :: () => (Show x, OpenSum.Member x PrimVal) => PrimDist x -> PrimDist x
pattern PrimDistPrf d <- d@(primDistPrf -> IsPrimVal)

-- | Proof that all primitive distributions generate a primitive value
primDistPrf :: PrimDist x -> IsPrimVal x
primDistPrf d = case d of
  HalfCauchyDist {}    -> IsPrimVal
  CauchyDist {}        -> IsPrimVal
  NormalDist {}        -> IsPrimVal
  HalfNormalDist  {}   -> IsPrimVal
  UniformDist  {}      -> IsPrimVal
  DiscrUniformDist {}  -> IsPrimVal
  GammaDist {}         -> IsPrimVal
  BetaDist {}          -> IsPrimVal
  BinomialDist {}      -> IsPrimVal
  BernoulliDist {}     -> IsPrimVal
  CategoricalDist {}   -> IsPrimVal
  DiscreteDist {}      -> IsPrimVal
  PoissonDist {}       -> IsPrimVal
  DirichletDist {}     -> IsPrimVal
  DeterministicDist {} -> IsPrimVal

-- | For erasing the types of primitive distributions
data ErasedPrimDist where
  ErasedPrimDist :: forall a. Show a => PrimDist a -> ErasedPrimDist

instance Show ErasedPrimDist where
  show (ErasedPrimDist d) = show d

-- | Dist a value from a primitive distribution in the @Sampler@ monad
dist :: (Has Sampler sig m)
  => PrimDist a
  -> m a
dist (HalfCauchyDist σ )
  = abs <$> createSampler (sampleCauchy 0 σ)
dist (CauchyDist μ σ )
  = createSampler (sampleCauchy μ σ)
dist (HalfNormalDist σ )
  = abs <$> createSampler (sampleNormal 0 σ)
dist (NormalDist μ σ )
  = createSampler (sampleNormal μ σ)
dist (UniformDist min max )
  = createSampler (sampleUniform min max)
dist (DiscrUniformDist min max )
  = createSampler (sampleDiscreteUniform min max)
dist (GammaDist k θ )
  = createSampler (sampleGamma k θ)
dist (BetaDist α β  )
  = createSampler (sampleBeta α β)
dist (BinomialDist n p  )
  = length . filter (== True) <$> createSampler (sampleBinomial n p)
dist (BernoulliDist p )
  = createSampler (sampleBernoulli p)
dist (CategoricalDist ps )
  =  fst . (ps !!) <$> createSampler (sampleCategorical $ V.fromList $ fmap snd ps)
dist (DiscreteDist ps )
  = createSampler (sampleDiscrete ps)
dist (PoissonDist λ )
  = createSampler (samplePoisson λ)
dist (DirichletDist xs )
  = createSampler (sampleDirichlet xs)
dist (DeterministicDist x) = pure x

-- | Compute the density of a primitive distribution generating an observed value
prob ::
     PrimDist a -- ^ distribution
  -> a          -- ^ observed value
  -> Double     -- ^ density
prob (DirichletDist xs) ys
  | Prelude.sum xs' /= 1 = error "dirichlet can't normalize"
  | otherwise
    = case dirichletDistribution (UV.fromList xs')
      of Left e  -> error "dirichlet error"
         Right d -> let Exp p = dirichletDensity d (UV.fromList ys) in exp p
  where
    xs' = map (/Prelude.sum xs) xs
prob (HalfCauchyDist σ) y
  | y < 0     = 0
  | otherwise = 2 * density (cauchyDistribution 0 σ) y
prob (CauchyDist μ σ) y
  = density (cauchyDistribution μ σ) y
prob (HalfNormalDist σ) y
  | y < 0 = 0
  | otherwise = 2 * density (normalDistr 0 σ) y
prob (NormalDist μ σ) y
  = density (normalDistr μ σ) y
prob (UniformDist min max) y
  = density (uniformDistr min max) y
prob (GammaDist k θ) y
  = density (gammaDistr k θ) y
prob  (BetaDist α β) y
  = density (betaDistr α β) y
prob (DiscrUniformDist min max) y
  = probability (discreteUniformAB min max) y
prob (BinomialDist n p) y
  = probability (binomial n p) y
prob (BernoulliDist p) i
  = probability (binomial 1 p) (fromEnum i)
prob d@(CategoricalDist ps) y
  = case lookup y ps of
      Nothing -> error $ "Couldn't find " ++ show y ++ " in categorical dist"
      Just p  -> p
prob (DiscreteDist ps) y     = ps !! y
prob (PoissonDist λ) y       = probability (poisson λ) y
prob (DeterministicDist x) y = 1

-- | Compute the log density of a primitive distribution generating an observed value
logProb :: PrimDist a -- ^ distribution
  -> a                -- ^ observed value
  -> Double           -- ^ log density
logProb d = log . prob d

{- $Address
   Run-time identifiers for probabilistic operations
-}

-- | An observable variable name assigned to a primitive distribution, representing a compile-time identifier
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence, representing a run-time identifier
type Addr = (Tag, Int)
