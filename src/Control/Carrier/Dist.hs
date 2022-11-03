{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

import Control.Algebra
import Control.Effect.Dist (Dist(..))
import Control.Effect.Sample (Sample(..))
import Control.Effect.Observe (Observe)
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

newtype DistC a m b = DistC { runDistC :: m b }
    deriving (Functor, Applicative, Monad)

instance forall sig m a . (Algebra sig m, Has (Sample a) sig m, Has (Observe a) sig m) => Algebra (Dist a :+: sig) (DistC a m) where
  alg :: forall ctx n . (Algebra sig m, Functor ctx)
      => Handler ctx n (DistC a m)
      -> (Dist a :+: sig) n a
      -> ctx ()
      -> DistC a m (ctx a)
  alg = loop 0 Map.empty
    where
      loop counter tagMap hdl sig@(L (Dist primDist mObs mTag)) ctx = DistC $ case mObs of
        Just obs -> undefined
        Nothing -> do
          x <- send (Sample primDist (tag, tagIdx))
          ctx' <- runDistC $ loop (counter + 1) tagMap' hdl sig ctx
          pure $ x <$ ctx'
        where
          tag     = fromMaybe (show counter) mTag
          tagIdx  = Map.findWithDefault 0 tag tagMap
          tagMap' = Map.insert tag (tagIdx + 1) tagMap

      loop counter tagMap hdl (R other) ctx = DistC $ alg hdl' other ctx
        where
          hdl' :: Handler ctx n m
          hdl' x = runDistC (hdl x)

              -- do 
              -- x <- send (Observe primDist obs (tag, tagIdx))
              -- ctx' <- runDist $ loop (counter + 1) tagMap' (hdl) sig ctx
              -- pure $ x <$ ctx