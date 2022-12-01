{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}



module Control.Carrier.SampTracer (SampTracerC, runSampTracer) where
import           Control.Algebra            (Algebra (..), Handler, Has, send)
import           Control.Carrier.State.Lazy (StateC, modify, runState)
import           Control.Effect.SampObs     (SampObs (..))
import           Control.Effect.State       (State, run)
import           Control.Effect.Sum         ((:+:) (..))
import           Control.Monad              (void, when)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Tuple                 (swap)
import           PrimDist                   (pattern PrimDistPrf)
import           Trace                      (STrace, updateSTrace)

newtype SampTracerC (m :: * -> *) (k :: *) = SampTracerC { runSampTracerC :: StateC STrace m k }
  deriving (Functor, Applicative, Monad)

runSampTracer :: Functor m => SampTracerC m k -> m (k, STrace)
runSampTracer = fmap swap . runState Map.empty . runSampTracerC

instance (Algebra (SampObs :+: sig) m) => Algebra (SampObs :+: sig) (SampTracerC m) where
    alg hdl sig ctx = SampTracerC $ case sig of
      L d@(SampObs (PrimDistPrf primDist) obs addr) -> do
        -- Perform the sample
        x <- send (SampObs primDist obs addr)

        -- Update the trace
        when (isNothing obs) $ modify (updateSTrace addr primDist x)

        -- Return the value
        pure $ x <$ ctx

      R other -> alg (runSampTracerC . hdl) (R (R other)) ctx
