{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes #-}

{- | The effect for reading observable variables from a model environment.
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
import           Env                        (Assign (..), Env, ObsVar)
import           GHC.Base                   (Symbol)

data ObsReaderC (e :: Assign Symbol *) (m :: * -> *) (k :: *) where
    ObsReaderC :: { runObsReaderC :: StateC [a] m k } -> ObsReaderC (x := a) m k

instance Functor m => Functor (ObsReaderC e m) where
    fmap f (ObsReaderC m) = ObsReaderC $ fmap f m

instance Applicative m => Applicative (ObsReaderC e m) where
    pure x = ObsReaderC $ pure x
    (ObsReaderC f) <*> (ObsReaderC x) = ObsReaderC $ f <*> x

instance Monad m => Monad (ObsReaderC e m) where
    (ObsReaderC m) >>= f = ObsReaderC $ m >>= (\(ObsReaderC m) -> f m)

runObsReader :: Functor m => [a] -> ObsReaderC (x ':= a) m k -> m k
runObsReader vs (ObsReaderC runObsReaderC) = evalState vs runObsReaderC

instance (Algebra sig m) => Algebra (ObsReader (x ':= a) :+: sig) (ObsReaderC (x ':= a) m) where
    alg hdl sig ctx = ObsReaderC $ case sig of
        L Ask -> do
            -- Use nested state effect to get current env
            vs <- State.get @[a]

            case vs of
                -- No values for x: return Nothing
                [] -> pure $ Nothing <$ ctx
                -- Values for x: pop and return first value
                (v : vt) -> do
                    State.put vt
                    pure $ Just v <$ ctx

        R other -> alg (\(ObsReaderC m) -> hdl m) (R other) ctx