{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module FusedEffects.ObsReader
  (ObsReader(..))
  where

import Env (Observable, ObsVar, Env, get, Observables, Assign)
import Data.Kind (Type)
import Control.Algebra ( Has, send, Algebra, type (:+:) (L, R), alg )
import Data.Maybe (listToMaybe)
import GHC.Base (Symbol)
import Control.Monad.Reader

-- //| Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
-- //^ variable @x@ to read from
-- //^ the head value from @x@'s list

data ObsReader (env :: [Assign Symbol Type]) (m :: Type -> Type) (k :: Type) where
  Ask :: Observable env x a => ObsVar x -> ObsReader env m (Maybe a)

ask :: forall env sig m x a. (Has (ObsReader env) sig m, Observable env x a) => ObsVar x -> m (Maybe a)
ask x = send (Ask @env x)

-- newtype ObsReaderC r m a = ObsReaderC (Env r -> 
newtype ObsReaderC env m a = ObsReaderC { runObsReader :: ReaderT env m a }
  deriving newtype (Applicative, Functor, Monad)

-- instance Functor (ObsReaderC env m a) where

--   deriving (Applicative, Functor, Monad)

-- runObsReader :: Env env -> ObsReaderC env m a -> m a
-- runObsReader r (ObsReaderC runReaderC) = runReaderC r

instance forall env sig m. Algebra sig m => Algebra (ObsReader env :+: sig) (ObsReaderC (Env env) m)
 where
  alg hdl sig ctx = ObsReaderC $ case sig of
    L (Ask x) -> let vs       = get x @env
                     maybe_v  = listToMaybe vs
                     env'     = set x (drop 1 vs) env
                 in  handleRead env' (k maybe_v)
    R other       -> alg (runObsReader . hdl) other ctx

-- action :: (Observables env '["p"] Double, Has (ObsReader Env) sig m) => m Double
-- action = do
--   observe (Env env)

-- main :: IO ()
-- main = putStrLn "Hello World"