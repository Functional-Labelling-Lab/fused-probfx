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


module Control.Carrier.SampleTracer where
import           Control.Algebra            (Algebra (..), Handler, Has, send)
import           Control.Carrier.State.Lazy (StateC, modify)
import           Control.Effect.Labelled    ((:+:) (..))
import           Control.Effect.Sample      (Sample (..), sample)
import           Control.Effect.State       (State)
import           PrimDist                   (pattern PrimDistPrf)
import           Trace                      (STrace, updateSTrace)

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
