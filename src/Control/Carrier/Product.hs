{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Carrier.Product (ProductC, runProduct) where

import           Control.Algebra            (Algebra (..), Handler, Has, send)
import           Control.Carrier.State.Lazy (StateC, modify, runState)
import           Control.Effect.State       (State, run)
import           Control.Effect.Sum         ((:+:) (..))
import           Data.WorldPeace.Product    (Product (..))

data ProductC
    (e :: u -> (* -> *) -> * -> *)
    (c :: u -> (* -> *) -> * -> *)
    (as :: [u]) (m :: * -> *) (k :: *) where
  CNil :: {runCNil :: m k} -> ProductC e c '[] m k
  CCons :: {runCCons ::  (c a (ProductC e c as m)) k} -> ProductC e c (a : as) m k

instance Functor m => Functor (ProductC e c '[] m) where
  fmap f (CNil m)  = CNil $ fmap f m


instance Functor (c a (ProductC e c as m)) => Functor (ProductC e c (a ': as) m) where
  fmap f (CCons c) = CCons $ fmap f c

instance {-# OVERLAPPABLE #-} Functor m => Functor (ProductC e c as m) where

instance Applicative m => Applicative (ProductC e c '[] m) where
  pure = CNil . pure
  (CNil f) <*> (CNil a) = CNil $ f <*> a

instance Applicative (c a (ProductC e c as m)) => Applicative (ProductC e c (a ': as) m) where
  pure = CCons . pure
  (CCons f) <*> (CCons a) = CCons $ f <*> a

instance {-# OVERLAPPABLE #-} Applicative m => Applicative (ProductC e c as m) where

instance Monad m => Monad (ProductC e c '[] m) where
  (>>=) (CNil m) f = CNil $ m >>= runCNil . f

instance Monad (c a (ProductC e c as m)) => Monad (ProductC e c (a ': as) m) where
  (>>=) (CCons m) f = CCons $ m >>= runCCons . f

instance {-# OVERLAPPABLE #-} Monad m => Monad (ProductC e c as m) where


runProduct :: (Monad m) 
  => Product f as 
  -> (forall m' a'. Monad m' => f a' -> c a' m' k -> m' k) 
  -> ProductC e c as m k -> m k
runProduct Nil _ (CNil m)            = m
runProduct (Cons a p) runC (CCons m) = runProduct p runC (runC a m)

instance Algebra sig m => Algebra sig (ProductC e c '[] m) where
  alg hdl sig ctx = CNil $ alg (runCNil . hdl) sig ctx

instance Algebra (e a :+: sig) (c a (ProductC e c as m))
    => Algebra (e a :+: sig) (ProductC e c (a ': as) m) where
  alg hdl sig ctx = CCons $ alg (runCCons . hdl) sig ctx
