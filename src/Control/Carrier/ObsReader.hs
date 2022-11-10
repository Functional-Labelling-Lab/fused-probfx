{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{- | The effect for reading observable variables from a model environment.
-}

module Control.Carrier.ObsReader (
    ObsReaderC(..),
    runObsReader
) where

import           Control.Algebra
import           Control.Carrier.State.Lazy (StateC, evalState, runState)
import           Control.Effect.ObsReader   (ObsReader (..))
import           Control.Effect.State       (State)
import qualified Control.Effect.State       as State
import           Data.Kind                  (Type)
import           Data.Maybe                 (listToMaybe)
import           Env                        (Assign, Env, ObsVar,
                                             Observable (..))
import           GHC.Base                   (Symbol)

newtype ObsReaderC (env :: [Assign Symbol *]) m k = ObsReaderC { runObsReaderC :: StateC (Env env) m k }
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
