{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Sampling randomly with randomly generated seed via IO.
-}

module Control.Carrier.Sampler (
    SamplerC,
    runSampler,
    runSamplerCustom
) where
  
import Control.Effect.Sampler (Sampler (..))
import Control.Algebra (Has, Algebra (alg))
import Control.Effect.Sum (type (:+:))
import Control.Effect.SampObs (SampObs)
import Control.Effect.Lift (Lift)
import qualified System.Random.MWC as MWC
import Control.Carrier.Lift (LiftC, runM)
import qualified Data.Vector as V
import Control.Carrier.Lift (sendM)
import Control.Algebra ((:+:)(..))
import Data.Functor (($>))
import Control.Algebra.Handler (Handler)
import Data.Word (Word32)
import Control.Carrier.Reader (ReaderC)
import Control.Carrier.Reader (runReader)
import Control.Effect.Reader (ask)

newtype SamplerC m k = SamplerC { runSamplerC :: ReaderC MWC.GenIO m k }
  deriving (Functor, Applicative, Monad)

runSampler :: (Has (Lift IO) sig m) => SamplerC m k -> m k
runSampler (SamplerC sampler) = do
  gen <- sendM @IO MWC.createSystemRandom
  runReader gen sampler

runSamplerCustom :: (Has (Lift IO) sig m) => Int -> SamplerC m k -> m k
runSamplerCustom seed (SamplerC sampler) = do
  gen <- sendM @IO $ MWC.initialize (V.singleton (fromIntegral seed :: Word32))
  runReader gen sampler

instance (Algebra sig m, Has (Lift IO) sig m) => Algebra (Sampler :+: sig) (SamplerC m) where
  alg hdl sig ctx = SamplerC $ case sig of
        L (Sampler f) -> do
          gen <- ask

          x <- sendM $ f gen

          pure $ x <$ ctx

        R other -> alg (runSamplerC . hdl) (R other) ctx
