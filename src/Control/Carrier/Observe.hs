{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Control.Carrier.Observe where

import Control.Algebra (Algebra (alg))
import Control.Effect.Sum (type (:+:))
import Control.Effect.Dist (Dist)
import Control.Algebra ((:+:)(..))
import Control.Effect.Observe (Observe(..))
import Control.Algebra.Handler (Handler)


newtype ObserveC (m :: * -> *) (k :: *) = ObserveC { runObserveC :: m k }
    deriving (Functor, Applicative, Monad)


instance (Algebra sig m) => Algebra (Observe :+: sig) (ObserveC m) where
  alg hdl sig ctx = ObserveC $ case sig of
    L (Observe d y _) -> pure $ y <$ ctx
    R other -> alg (runObserveC . hdl) other ctx

