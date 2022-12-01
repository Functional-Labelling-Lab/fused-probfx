{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{- | The effect for reading observable variables from a model environment.
-}

module Control.Effect.EffUnion (
    EffUnion(..),
    FlipEffect(..),
    liftEffUnion
  ) where

import           Control.Algebra
import           Data.Maybe            (listToMaybe)
import           Data.WorldPeace.Extra (IsMember (..))
import qualified Data.WorldPeace.Union as Union
import           Env                   (Assign (..), ObsVar)
import           GHC.Types             (Symbol)
import Control.Effect.Sum (Member(inj))

newtype FlipEffect (e :: u -> (* -> *) -> * -> *) (m :: * -> *) (k :: *) (a :: u) = FlipEffect {runFlipEffect :: e a m k}

-- | The effect for reading observed values from a model environment @env@
data EffUnion (e :: u -> (* -> *) -> * -> *) (as :: [u]) (m :: * -> *) (k :: *) where
  EffUnion :: forall as e m k. (Union.Union (FlipEffect e m k) as -> EffUnion e as m k)

newtype LiftEffUnionC (e :: u -> (* -> *) -> * -> *) (a :: u) (as :: [u]) (sig :: (* -> *) -> * -> *) (m :: * -> *) (k :: *)
  = LiftEffUnionC {liftEffUnionC :: (Has (EffUnion e as) sig m) => m k}
  deriving (Functor)

liftEffUnion :: forall as e a sig m k. Has (EffUnion e as) sig m => LiftEffUnionC e a as sig m k -> m k
liftEffUnion m = liftEffUnionC m

instance (Has (EffUnion e as) sig m, Applicative m) => Applicative (LiftEffUnionC e a as sig m) where
  pure x = LiftEffUnionC $ pure x
  (LiftEffUnionC f) <*> (LiftEffUnionC x) = LiftEffUnionC $ f <*> x

instance (Has (EffUnion e as) sig m, Monad m) => Monad (LiftEffUnionC e a as sig m) where
  (LiftEffUnionC m) >>= f = LiftEffUnionC $ m >>= (liftEffUnion . f)

instance (IsMember a as, Has (EffUnion e as) sig m) => Algebra (e a :+: sig) (LiftEffUnionC e a as sig m) where
  alg :: forall u (a :: u) (as :: [u]) (e :: u -> (* -> *) -> * -> *)
       (sig :: (* -> *) -> * -> *) (m :: * -> *) (ctx :: * -> *)
       (n :: * -> *) a1.
    (IsMember a as, Has (EffUnion e as) sig m, Functor ctx) =>
    Handler ctx n (LiftEffUnionC e a as sig m)
    -> (:+:) (e a) sig n a1
    -> ctx ()
    -> LiftEffUnionC e a as sig m (ctx a1)
  alg hdl sig ctx = LiftEffUnionC $ case sig of
    (L eff) -> alg (liftEffUnion . hdl) (inj (EffUnion $ unionLift @a @as $ FlipEffect eff)) ctx
    (R other) -> alg (liftEffUnion . hdl) other ctx

