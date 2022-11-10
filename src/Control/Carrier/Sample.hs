{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Carrier.Sample where
import           Control.Algebra         (Algebra (alg), type (:+:))
import           Control.Effect.Labelled ((:+:) (..))
import           Control.Effect.Sample   (Sample (..))
import           PrimDist                (sample)
import           Sampler                 (Sampler)


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



