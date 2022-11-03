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

{- | The effect for reading observable variables from a model environment.
-}

module Control.Carrier.ObsReader (
    ObsReaderC(..),
    runObsReader
) where

import Control.Algebra
import Data.Kind (Type)
import Control.Effect.ObsReader (ObsReader(..))
import qualified Control.Effect.State as State
import Control.Carrier.State.Strict (StateC, runState, evalState)
import Env (Env, ObsVar, Observable(..), Assign)
import Data.Maybe (listToMaybe)

newtype ObsReaderC env m k = ObsReaderC { runObsReaderC :: StateC (Env env) m k }
    deriving (Functor, Applicative, Monad)

runObsReader :: Functor m => Env env -> ObsReaderC env m a -> m a
runObsReader env = evalState env . runObsReaderC

instance (Algebra sig m) => Algebra (ObsReader env :+: sig) (ObsReaderC env m) where
    alg hdl sig ctx = ObsReaderC $ case sig of
        L (Ask x) -> do
            -- Use nested state effect to get current env
            env <- State.get @(Env env)

            case get x env of
                -- No values for x: return Nothing
                [] -> pure $ Nothing <$ ctx
                -- Values for x: pop and return first value
                (v : vs) -> do
                    State.put $ set x vs env
                    pure $ Just v <$ ctx
    
        R other -> alg (runObsReaderC . hdl) (R other) ctx
