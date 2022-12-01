{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Control.Carrier.EffUnion (EffUnionC, runEffUnion) where

import           Control.Algebra            (Algebra (..), Handler, Has, send)
import           Control.Carrier.State.Lazy (StateC, modify, runState)
import           Control.Effect.EffUnion    (EffUnion (..), FlipEffect (..))
import           Control.Effect.State       (State, run)
import           Control.Effect.Sum         (Member (inj), (:+:) (..))
import qualified Data.WorldPeace            as WP
import           Data.WorldPeace.Extra (IsMember)

data EffUnionC
    (e :: u -> (* -> *) -> * -> *)
    (c :: u -> (* -> *) -> * -> *)
    (as :: [u]) (m :: * -> *) (k :: *) where
  CNil :: {runCNil :: m k} -> EffUnionC e c '[] m k
  CCons :: {runCCons :: (c a (EffUnionC e c as m)) k} -> EffUnionC e c (a : as) m k

instance (
      Functor m,
      forall a' as' m'. (Functor m' => Functor (c a' m'))
    ) => Functor (EffUnionC e c as m) where
  fmap f (CNil m)  = CNil $ fmap f m
  fmap f (CCons m) = CCons $ fmap f m

instance (
      Applicative m,
      forall a' as' m'. (Functor m' => Functor (c a' m'))
    ) => Applicative (EffUnionC e c '[] m) where
  pure x = CNil $ pure x
  CNil f <*> CNil m = CNil $ f <*> m

instance (
      Applicative m,
      forall a' m'. (Applicative m' => Applicative (c a' m')),
      forall a' as' m'. (Functor m' => Functor (c a' m')),
      Applicative (EffUnionC e c as m)
    ) => Applicative (EffUnionC e c (a : as) m) where
  pure x = CCons $ pure x
  CCons f <*> CCons m = CCons $ f <*> m

instance {-# INCOHERENT #-} (
      Applicative m,
      forall a' m'. (Applicative m' => Applicative (c a' m')),
      forall a' as' m'. (Functor m' => Functor (c a' m'))
    ) => Applicative (EffUnionC e c as m) where

instance (
      Monad m,
      forall a' m'. (Monad m' => Monad (c a' m')),
      forall a' m'. (Applicative m' => Applicative (c a' m')),
      forall a' m'. (Functor m' => Functor (c a' m'))
    ) => Monad (EffUnionC e c as m) where
  CNil m >>= f  = CNil $ m >>= (runCNil . f)
  CCons m >>= f = CCons $ m >>= (runCCons . f)

runEffUnion :: (
    Monad m,
    forall a' m'. (Monad m' => Monad (c a' m')),
    forall a' m'. (Applicative m' => Applicative (c a' m')),
    forall a' as' m'. (Functor m' => Functor (c a' m'))
  )
  => WP.Product f as
  -> (forall m' a'. Monad m' => f a' -> c a' m' k -> m' k)
  -> EffUnionC e c as m k -> m k
runEffUnion WP.Nil _ (CNil m)            = m
runEffUnion (WP.Cons a p) runC (CCons m) = runEffUnion p runC (runC a m)


-- A bit hacky since it'll never get to that stage since it'll be handled by the WP.This case but for the sake of type-checker
instance (Algebra sig m, 
          Monad (EffUnionC e c '[] m)
          ) => Algebra (EffUnion e '[] :+: sig) (EffUnionC e c '[] m) where
  alg hdl sig ctx = CNil $ case sig of
    L (EffUnion u) -> WP.absurdUnion u
    R other        -> alg (runCNil . hdl) other ctx

instance (Algebra sig m, 
          Algebra (e a :+: (EffUnion e as :+: sig)) (c a (EffUnionC e c as m)),
          Monad (EffUnionC e c (a : as) m)
          ) => Algebra (EffUnion e (a : as) :+: sig) (EffUnionC e c (a : as) m) where
  alg hdl sig ctx = CCons $ case sig of
    L (EffUnion (WP.This (FlipEffect eff))) -> alg (runCCons . hdl) (L eff) ctx
    L (EffUnion (WP.That u)) -> alg (runCCons . hdl) (R $ L $ EffUnion u) ctx
    R other -> alg (runCCons . hdl) (R (R other)) ctx

instance {-# INCOHERENT #-} (
          Algebra sig m,
          forall a' as' sig' m'. ((IsMember a' as', Algebra sig' m') => Algebra (e a' :+: (EffUnion e as' :+: sig')) (c a' (EffUnionC e c as' m'))),
          Monad (EffUnionC e c as m)) => Algebra (EffUnion e as :+: sig) (EffUnionC e c as m) where