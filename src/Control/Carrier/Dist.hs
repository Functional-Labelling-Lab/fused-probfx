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
import Effects.Dist (Tag)
import Control.Effect.Observe (Observe(..))

newtype DistC a m b = DistC { runDistC :: m b }
    deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Has (Sample b) sig m, Has (Observe b) sig m) => Algebra (Dist b :+: sig) (DistC b m) where
  alg :: forall ctx n a . Functor ctx
      => Handler ctx n (DistC b m) --
      -> (Dist b :+: sig) n a
      -> ctx ()
      -> DistC b m (ctx a)
  alg = loop 0 Map.empty
    where
      loop
          :: Int
          -> Map.Map Tag Int
          -> Handler ctx n (DistC b m) --
          -> (Dist b :+: sig) n a
          -> ctx ()
          -> DistC b m (ctx a)
      loop counter tagMap hdl sig@(L (Dist primDist mObs mTag)) ctx = DistC $ case mObs of
        Just obs -> do 
          x <- send (Observe primDist obs (tag, tagIdx))
          ctx' <- recurCtx
          pure $ x <$ ctx'
        Nothing -> do
          x <- send (Sample primDist (tag, tagIdx))
          ctx' <- recurCtx
          pure $ x <$ ctx'
        where
          tag     = fromMaybe (show counter) mTag
          tagIdx  = Map.findWithDefault 0 tag tagMap
          tagMap' = Map.insert tag (tagIdx + 1) tagMap
          recurCtx = runDistC $ loop (counter + 1) tagMap' (hdl) sig ctx

      loop counter tagMap hdl (R other) ctx = DistC $ alg hdl' other ctx
        where
          hdl' :: Handler ctx n m
          hdl' x = runDistC (hdl x)
