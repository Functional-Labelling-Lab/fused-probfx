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

import Control.Algebra
import Data.Kind (Type)
import Control.Effect.ObsReader (ObsReader(..))
import qualified Control.Effect.State as State
import Control.Carrier.State.Church (StateC)
import Env (Env, ObsVar, Observable(..), Assign)
import Data.Maybe (listToMaybe)

newtype ObsReaderC env m k = ObsReaderC { runObsReaderC :: StateC (Env env) m k }
    deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (ObsReader env :+: sig) (ObsReaderC env m) where
    alg hdl sig ctx = ObsReaderC $ case sig of
        L (Ask x) -> do
            env <- State.get @(Env env)
            let vs       = get x env
                maybe_v  = listToMaybe vs
                env'     = set x (drop 1 vs) env
            State.put env'
            pure $ maybe_v <$ ctx
        
        R other   -> alg (runObsReaderC . hdl) (R other) ctx
