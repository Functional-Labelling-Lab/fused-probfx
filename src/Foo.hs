{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Algebra (Has)
import Control.Carrier.Choose.Church (Choose, runChoose)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Control.Effect.Labelled (send, Algebra (alg))
import Control.Effect.Choose (Choose(..))
import Control.Algebra (Algebra)
import Control.Algebra (type (:+:))
import Control.Algebra (type (:+:)(L))
import Control.Algebra (type (:+:)(R))
import Data.Functor (($>))
import Control.Algebra (run)


foo :: (Has Choose sig m) => m Int
foo = do
  x <- send $ Choose
  return $ if x then 1 else 2


bar :: Identity Int
bar = runChoose (\x y -> Identity $ runIdentity x + runIdentity y) Identity foo

data Foo (m :: * -> *) k where
  AskFoo :: Foo m Int

askFoo :: (Has Foo sig m) => m Int 
askFoo = send AskFoo

newtype FooC m k = FooC {runFoo :: m k}
  deriving (Applicative, Functor, Monad)

-- runFoo :: FooC m k -> m k
-- runFoo (FooC runFooC) = runFooC
  

instance Algebra sig m => Algebra (Foo :+: sig) (FooC m) where
  alg hdl sig ctx = FooC $ case sig of
    L AskFoo -> pure $ 5 <$ ctx
    R other  -> alg (runFoo . hdl) other ctx

action :: (Has Foo sig m) => m Int
action = do
  askFoo

main :: IO ()
main = do
  print $ run $ runFoo $ action
