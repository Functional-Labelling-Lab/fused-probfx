{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

{- | The effect for reading observable variables from a model environment.
-}

module Effects.ObsReader (
    ObsReader(..),
    ObsReaderC(..),
    runObsReader,
    handleRead
  , ask ) where

import Prog ( call, discharge, Member, Prog(..) )
import Env ( Env, ObsVar, Observable(..), Assign )
import Data.Maybe (listToMaybe)
import Data.Kind (Type)
import Control.Algebra (Has, Algebra(..),type  (:+:) (R, L), send)
import GHC.Base (Symbol)
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Applicative (Applicative(liftA2))
import qualified Control.Algebra as Control.Algebra.Handler
data ObsReader (env :: [Assign Symbol Type]) (m :: Type -> Type) k where
  Ask :: Observable env x a => ObsVar x -> ObsReader env m (Maybe a)

-- | The effect for reading observed values from a model environment @env@
-- data ObsReader env a where
--   -- | Given the observable variable @x@ is assigned a list of type @[a]@ in @env@, attempt to retrieve its head value.
--   Ask :: Observable env x a
--     => ObsVar x                 -- ^ variable @x@ to read from
--     -> ObsReader env (Maybe a)  -- ^ the head value from @x@'s list

-- | Wrapper function for calling @Ask@
ask :: forall env sig m x a . (Has (ObsReader env) sig m, Observable env x a)
  => ObsVar x -> m (Maybe a)
ask x = send (Ask @env x)

-- newtype ObsReaderC env m k = ObsReaderC { runObsReader :: env -> m k }
--   deriving (Functor)

newtype ObsReaderC env m k = ObsReaderC (Env env -> m k)
  deriving (Functor)

runObsReader :: Env env -> ObsReaderC env m k -> m k
runObsReader env (ObsReaderC runObsReader) = runObsReader env

  -- deriving (Applicative, Functor, Monad)

instance Applicative m => Applicative (ObsReaderC env m) where
  pure = ObsReaderC . const . pure

  ObsReaderC f <*> ObsReaderC a = ObsReaderC (liftA2 (<*>) f a)

  liftA2 f (ObsReaderC a) (ObsReaderC b) = ObsReaderC $ \ r ->
    liftA2 f (a r) (b r)

instance Monad m => Monad (ObsReaderC env m) where
  (>>=) :: Monad m => ObsReaderC env m a -> (a -> ObsReaderC env m b) -> ObsReaderC env m b
  o >>= f = ObsReaderC $ \ env -> do
    x <- runObsReader env o
    runObsReader env (f x)

instance Algebra sig m => Algebra (ObsReader env :+: sig) (ObsReaderC env m) where
  alg :: (Algebra sig m, Functor ctx)
      => Control.Algebra.Handler.Handler ctx n (ObsReaderC env m)
      -> (:+:) (ObsReader env) sig n a
      -> ctx ()
      -> ObsReaderC env m (ctx a)
  alg hdl sig ctx = ObsReaderC $ \env -> case sig of
    L (Ask x) -> case get x env of
        [] -> pure $ Nothing <$ ctx
        (v : vs) -> do
          pure $ Just v <$ ctx -- TODO DOES NOT REMOVE VALUE FROM ENV
    R other  -> alg (runObsReader env . hdl) other ctx

-- | Handle the @Ask@ requests of observable variables
-- handleRead ::
--   -- | initial model environment
--      Env env
--   -> Prog (ObsReader env ': es) a
--   -> Prog es a
-- handleRead env (Val x) = return x
-- handleRead env (Op op k) = case discharge op of
--   Right (Ask x) -> case get x env of
--     [] -> handleRead env (k Nothing)
--     (v : vs) -> handleRead (set x vs env) (k $ Just v)
--   Left op' -> Op op' (handleRead env . k)

handleRead = undefined