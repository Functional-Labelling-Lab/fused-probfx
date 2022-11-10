{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Carrier.Observe where

import           Control.Algebra         (Algebra (alg), (:+:) (..))
import           Control.Algebra.Handler (Handler)
import           Control.Effect.Dist     (Dist)
import           Control.Effect.Observe  (Observe (..))
import           Control.Effect.Sum      (type (:+:))

newtype ObserveC (m :: * -> *) (k :: *) = ObserveC { runObserveC :: m k }
    deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (Observe :+: sig) (ObserveC m) where
  alg hdl sig ctx = ObserveC $ case sig of
    L (Observe d y _) -> pure $ y <$ ctx
    R other           -> alg (runObserveC . hdl) other ctx

