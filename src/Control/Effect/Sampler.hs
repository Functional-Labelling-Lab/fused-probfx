{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

{- | An IO-based sampling monad.
-}

module Control.Effect.Sampler (
  -- * Sampler monad
    Sampler(..)
  -- , liftS
  -- , sampleIO
  -- , sampleIOFixed
  , createSampler
  -- , sampleIOCustom
  -- * Sampling functions
  -- $Sampling-functions
  , sampleRandom
  , sampleCauchy
  , sampleNormal
  , sampleUniform
  , sampleDiscreteUniform
  , sampleGamma
  , sampleBeta
  , sampleBernoulli
  , sampleBinomial
  , sampleCategorical
  , sampleDiscrete
  , samplePoisson
  , sampleDirichlet
  ) where

import           Control.Monad                         (replicateM)
import           Control.Monad.Trans                   (MonadIO, MonadTrans,
                                                        lift)
import           Control.Monad.Trans.Reader            (ReaderT, ask,
                                                        mapReaderT, runReaderT)
import           Data.Map                              (Map)
import qualified Data.Vector                           as V
import           GHC.Word                              (Word32)
import           Statistics.Distribution               (ContGen (genContVar))
import           Statistics.Distribution.CauchyLorentz (cauchyDistribution)
import qualified System.Random.MWC                     as MWC
import           System.Random.MWC                     (initialize)
import qualified System.Random.MWC.Distributions       as MWC.Dist
import qualified System.Random.MWC.Probability         as MWC.Probability
import Control.Algebra (Has, send)
import Control.Effect.Lift (Lift)
import Control.Carrier.Lift (sendM)


-- | Sampler effect, for running IO computations alongside a random number generator
data Sampler (m :: * -> *) k where
  Sampler :: (MWC.GenIO -> IO k) -> Sampler m k

-- | Takes a @Sampler@, provides it a random generator, and runs the sampler in the @IO@ context
-- sampleIO :: Sampler a -> IO a
-- sampleIO m = MWC.createSystemRandom >>= (runReaderT . runSampler) m

-- -- | Takes a @Sampler@, provides it a fixed generator, and runs the sampler in the @IO@ context
-- sampleIOFixed :: Sampler a -> IO a
-- sampleIOFixed m = MWC.create >>= (runReaderT . runSampler) m

-- -- | Takes a @Sampler@, provides it a custom fixed generator, and runs the sampler in the @IO@ context
-- sampleIOCustom :: Int -> Sampler a -> IO a
-- sampleIOCustom n m = initialize (V.singleton (fromIntegral n :: Word32)) >>= (runReaderT . runSampler) m

-- | Takes a distribution which awaits a generator, and returns a @Sampler@
-- createSampler :: Has Sampler sig m => (MWC.GenIO -> IO a) -> m a
-- createSampler f = send $ Sampler $ ask >>= lift . f

createSampler :: (Has Sampler sig m) => (MWC.GenIO -> IO a) -> m a
createSampler = send . Sampler
-- createSampler f = Sampler $ ask >>= lift . f

{- $Sampling-functions
  Given their distribution parameters, these functions await a generator and
  then sample a value from the distribution in the @IO@ monad.
-}

sampleRandom
  :: MWC.GenIO
  -> IO Double
sampleRandom = MWC.uniform

sampleCauchy
  :: Double -- ^ location
  -> Double -- ^ scale
  -> (MWC.GenIO -> IO Double)
sampleCauchy μ σ = genContVar (cauchyDistribution μ σ)

sampleNormal
  :: Double -- ^ mean
  -> Double -- ^ standard deviation
  -> (MWC.GenIO -> IO Double)
sampleNormal = MWC.Dist.normal

sampleUniform
  :: Double -- ^ lower-bound
  -> Double -- ^ upper-bound
  -> (MWC.GenIO -> IO Double)
sampleUniform min max = MWC.uniformR (min, max)

sampleDiscreteUniform
  :: Int -- ^ lower-bound
  -> Int -- ^ upper-bound
  -> (MWC.GenIO -> IO Int)
sampleDiscreteUniform min max = MWC.uniformR (min, max)

sampleGamma
  :: Double -- ^ shape k
  -> Double -- ^ scale θ
  -> (MWC.GenIO -> IO Double)
sampleGamma = MWC.Dist.gamma

sampleBeta
  :: Double -- ^ shape α
  -> Double -- ^ shape β
  -> (MWC.GenIO -> IO Double)
sampleBeta = MWC.Dist.beta

sampleBernoulli
  :: Double -- ^ probability of @True@
  -> (MWC.GenIO -> IO Bool)
sampleBernoulli = MWC.Dist.bernoulli

sampleBinomial
  :: Int    -- ^ number of trials
  -> Double -- ^ probability of successful trial
  -> (MWC.GenIO -> IO [Bool])
sampleBinomial n p gen = replicateM n (MWC.Dist.bernoulli p gen)

sampleCategorical
  :: V.Vector Double -- ^ probabilities
  -> (MWC.GenIO -> IO Int)
sampleCategorical = MWC.Dist.categorical

sampleDiscrete
  :: [Double] -- ^ probabilities
  -> (MWC.GenIO -> IO Int)
sampleDiscrete ps = MWC.Dist.categorical (V.fromList ps)

samplePoisson
  :: Double   -- ^ rate λ
  -> (MWC.GenIO -> IO Int)
samplePoisson λ = MWC.Probability.sample (MWC.Probability.poisson λ)

sampleDirichlet
  :: [Double] -- ^ concentrations
  -> (MWC.GenIO -> IO [Double])
sampleDirichlet = MWC.Dist.dirichlet
