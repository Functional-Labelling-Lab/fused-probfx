{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE OverloadedLabels           #-}

import           Control.Algebra           (Algebra (alg), Has, run, send,
                                            type (:+:) (L))
import           Control.Effect.Sum        (Member, type (:+:) (R))

-- import Effects.ObsReader (ObsReader (Ask), ask, ObsReaderC, runObsReader)
import           Control.Carrier.ObsReader (ObsReaderC, runObsReader)
import           Control.Effect.ObsReader  (ObsReader (..), ask)

import           Env                       (Assign ((:=)), Env, LookupType,
                                            ObsVar (ObsVar), Observable, nil,
                                            (<:>))

import qualified Control.Algebra           as Control.Algebra.Handler
import           Control.Monad.Reader      (ReaderT (runReaderT))
import           Data.Functor              (($>))
import           Data.Kind                 (Type)
import           GHC.Base                  (Symbol)

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
