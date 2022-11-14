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



module Control.Carrier.SampleTracer (SampleTracerC, runSampleTracer) where
import           Control.Algebra            (Algebra (..), Handler, Has, send)
import           Control.Carrier.State.Lazy (StateC, modify, runState)
import           Control.Effect.Dist        (Dist (..))
import           Control.Effect.State       (State, run)
import           Control.Effect.Sum         ((:+:) (..))
import           Control.Monad              (void, when)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Tuple                 (swap)
import           PrimDist                   (pattern PrimDistPrf)
import           Trace                      (STrace, updateSTrace)

newtype SampleTracerC (m :: * -> *) (k :: *) = SampleTracerC { runSampleTracerC :: StateC STrace m k }
  deriving (Functor, Applicative, Monad)

runSampleTracer :: Functor m => SampleTracerC m k -> m (k, STrace)
runSampleTracer = fmap swap . runState Map.empty . runSampleTracerC

instance (Has Dist (Dist :+: sig) m) => Algebra (Dist :+: sig) (SampleTracerC m) where
    alg :: (Has Dist (Dist :+: sig) m, Functor ctx)
      => Handler ctx n (SampleTracerC m)
      -> (Dist :+: sig) n a
      -> ctx ()
      -> SampleTracerC m (ctx a)
    alg hdl sig ctx = SampleTracerC $ case sig of
      L d@(Dist (PrimDistPrf primDist) obs addr) -> do
        -- Perform the sample
        x <- send (Dist primDist obs addr)

        -- Update the trace
        when (isNothing obs) $ modify (updateSTrace addr primDist x)

        -- Return the value
        pure $ x <$ ctx

      R other -> alg (runSampleTracerC . hdl) (R (R other)) ctx
