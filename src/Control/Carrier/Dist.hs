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

import Control.Algebra
import Control.Effect.Dist (Dist(..))
import Control.Effect.Sample (Sample)
import Control.Effect.Observe (Observe)
import Data.Kind (Type)

newtype DistC a m b = DistC { runDistC :: m b }
    deriving (Functor, Applicative, Monad)

instance (Has Sample sig m, Has Observe sig m, Algebra sig m) => Algebra (Dist a :+: sig) (DistC a m) where
    alg hdl sig ctx = DistC $ case sig of
        L (Dist d maybeY maybeTag) -> undefined -- Have to figure out this whole folding
        R other -> alg (runDistC . hdl) other ctx
