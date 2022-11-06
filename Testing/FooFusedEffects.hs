{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Control.Algebra (Has, send, Algebra (alg), type (:+:) (L), run)
import Control.Effect.Sum (type (:+:)(R), Member)

-- import Effects.ObsReader (ObsReader (Ask), ask, ObsReaderC, runObsReader)
import Control.Effect.ObsReader (ObsReader(..), ask)
import Control.Carrier.ObsReader (ObsReaderC, runObsReader)

import Env (Observable, Assign ((:=)), (<:>), nil, LookupType, ObsVar (ObsVar), Env)

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Control.Algebra as Control.Algebra.Handler
import Data.Functor (($>))
import GHC.Base (Symbol)
import Data.Kind (Type)

data Foo (m :: * -> *) k where
  AskFoo :: Foo m Int

askFoo :: (Has Foo sig m) => m Int 
askFoo = send AskFoo

newtype FooC m k = FooC {runFoo :: m k}
  deriving (Applicative, Functor, Monad)
  

instance Algebra sig m => Algebra (Foo :+: sig) (FooC m) where
  alg hdl sig ctx = FooC $ case sig of
    L AskFoo -> pure $ ctx $> 5
    R other  -> alg (runFoo . hdl) other ctx

action :: (Has Foo sig m) => m Int
action = do askFoo

main' :: IO ()
main' = do
  print $ run $ runFoo action

-------

action'
  :: forall env sig m. (Member (ObsReader env) sig, Algebra sig m, Observable env "x" Int)
  => ObsReaderC env m [Maybe Int]
action' = do
    x <- ask @env #x
    y <- ask @env #x
    return [Just 1, Just 2]

-- action'' :: ObsReaderC env m k

main :: IO ()
main = do
    let env' ::  Env '["x" ':= Int]
        env' = #x := [1 :: Int, 2] <:> nil

    let fuckoff :: LookupType "x" '["x" ':= Int]
        fuckoff = 5

    -- let s = run $ runObsReader env' action'

    print "Hello World"


-- #foo :: ObsVar "foo"
