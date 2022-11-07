{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Carrier.Sample where
import Control.Algebra (type (:+:), Algebra (alg))
import Control.Effect.Sample (Sample (..))
import Control.Effect.Labelled ((:+:)(..))
import Sampler (Sampler)
import PrimDist (sample)


-- special carrier: doesn't have an m because it must be the last
-- effect to be handled

-- If we want this to NOT be the last effect to be handled, we must
-- make it change the Sample effect into a (Lift IO) effect or something
-- which would involve changing things related to Sampler, so I was
-- going to leave it until later on in the migration.

newtype SampleC (k :: *) = SampleC { runSampleC :: Sampler k }
    deriving (Functor, Applicative, Monad)


instance Algebra Sample SampleC where
  alg hdl (Sample primDist addr) ctx = SampleC $ do
    x <- sample primDist
    pure $ x <$ ctx



