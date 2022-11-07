{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Control.Carrier.SampleTracer where
import Control.Carrier.State.Strict (StateC)
import Trace (STrace, updateSTrace)
import Control.Algebra (Algebra (..))
import Control.Effect.Labelled ((:+:)(..))
import Control.Effect.Sample (Sample (..))
import Control.Algebra (Has)
import Control.Carrier.Sample (SampleC(runSampleC))
import Control.Effect.State (modify)
import PrimDist (pattern PrimDistPrf)
import Control.Algebra (send)

newtype SampleTracerC (m :: * -> *) (k :: *) = SampleTracerC { runSampleTracerC :: StateC STrace m k }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Has Sample sig m) => Algebra (Sample :+: sig) (SampleTracerC m) where
    alg hdl sig ctx = SampleTracerC $ case sig of
        L s@(Sample (PrimDistPrf primDist) addr) -> do
          -- Perform the sample
          x <- undefined
          
          -- Update the trace
          modify (updateSTrace addr primDist x)

          -- Return the value
          pure $ x <$ ctx
    
        R other -> alg (runSampleTracerC . hdl) (R other) ctx
