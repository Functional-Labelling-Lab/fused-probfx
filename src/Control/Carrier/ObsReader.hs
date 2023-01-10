{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{- | Carrier for the 'Control.Effect.ObsReader' effect.
-}

module Control.Carrier.ObsReader (
    ObsReaderC,
    runObsReader
) where

import           Control.Algebra
import           Control.Carrier.State.Lazy (StateC, evalState, runState)
import           Control.Effect.ObsReader   (ObsReader (..))
import           Control.Effect.State       (State)
import qualified Control.Effect.State       as State
import           Data.Kind                  (Type)
import           Data.Maybe                 (listToMaybe)
import           Data.WorldPeace.Extra      (IsMember (productGet, productSet))
import           Env                        (Assign, Env, EnvElem (..), ObsVar,
                                             Observable, get, set)
import           GHC.Base                   (Symbol)

-- | Carrier for the top-level 'Control.Effect.ObsReader' effect that pops a value from
--   a list for the specified variable in the environment
newtype ObsReaderC (env :: [Assign Symbol *]) m k = ObsReaderC { runObsReaderC :: StateC (Env env) m k }
    deriving (Functor, Applicative, Monad)

-- | Executes the 'ObsReaderC' carrier with the specified environment
runObsReader :: Functor m => Env env -> ObsReaderC env m a -> m a
runObsReader env (ObsReaderC runObsReaderC) = evalState env runObsReaderC

instance (Algebra sig m) => Algebra (ObsReader env :+: sig) (ObsReaderC env m) where
    alg :: forall sig m ctx n env a x a'. (Algebra sig m, Functor ctx) =>
        Handler ctx n (ObsReaderC env m)
        -> (:+:) (ObsReader env) sig n a
        -> ctx ()
        -> ObsReaderC env m (ctx a)
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
