{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{- | Carrier for the 'Control.Effect.SampObs' effect for 
     log-probability calculation
-}

module Control.Carrier.LPTracer (LPTracerC, runLPTracer) where
import           Control.Algebra            (Algebra (..), Handler, Has, send)
import           Control.Carrier.Reader     (ReaderC, ask, runReader)
import           Control.Carrier.State.Lazy (StateC, modify, runState)
import           Control.Effect.SampObs     (SampObs (..))
import           Control.Effect.State       (State, run)
import           Control.Effect.Sum         ((:+:) (..))
import           Control.Monad              (void, when)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Tuple                 (swap)
import           PrimDist                   (pattern PrimDistPrf)
import           Trace                      (LPTrace, updateLPTrace)

-- | Carrier for the 'Control.Effect.SampObs' effect that captures a compounded
--   log-probability for each observation (or sampling if included)
newtype LPTracerC (m :: * -> *) (k :: *) = LPTracerC { runLPTracerC :: ReaderC Bool (StateC LPTrace m) k }
  deriving (Functor, Applicative, Monad)

-- | Executes the 'LPTracerC' carrier
runLPTracer :: Functor m
  => Bool           -- ^ Whether the sampling actions should be included as well
  -> LPTracerC m k
  -> m (k, LPTrace)
runLPTracer includeSample = fmap swap . runState Map.empty . runReader includeSample . runLPTracerC

instance (Has SampObs (SampObs :+: sig) m) => Algebra (SampObs :+: sig) (LPTracerC m) where
    alg hdl sig ctx = LPTracerC $ case sig of
      L d@(SampObs (PrimDistPrf primDist) obs addr) -> do
        -- Perform the sample
        x <- send (SampObs primDist obs addr)

        -- Update the trace
        includeSample <- ask
        when (isJust obs || includeSample) $ modify (updateLPTrace addr primDist x)

        -- Return the value
        pure $ x <$ ctx

      R other -> alg (runLPTracerC . hdl) (R (R (R other))) ctx
