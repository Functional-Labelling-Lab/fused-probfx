{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}


module Control.Carrier.SampleTracer where
import Control.Carrier.State.Lazy ( StateC, modify )
import Trace (STrace, updateSTrace)
import Control.Algebra ( Algebra(..), Has, send, Handler )
import Control.Effect.Labelled ((:+:)(..))
import Control.Effect.Sample ( Sample(..), sample )
import PrimDist (pattern PrimDistPrf)
import Control.Effect.State (State)

newtype SampleTracerC (m :: * -> *) (k :: *) = SampleTracerC { runSampleTracerC :: StateC STrace m k }
  deriving (Functor, Applicative, Monad)

instance (Has Sample (Sample :+: sig) m) => Algebra (Sample :+: sig) (SampleTracerC m) where
    alg :: (Has Sample (Sample :+: sig) m, Functor ctx) 
      => Handler ctx n (SampleTracerC m)
      -> (Sample :+: sig) n a 
      -> ctx () 
      -> SampleTracerC m (ctx a)
    alg hdl sig ctx = SampleTracerC $ case sig of
      L s@(Sample (PrimDistPrf primDist) addr) -> do
        -- Perform the sample
        x <- sample primDist addr

        -- Update the trace
        modify (updateSTrace addr primDist x)

        -- Return the value
        pure $ x <$ ctx

      R other -> alg (runSampleTracerC . hdl) (R (R other)) ctx
