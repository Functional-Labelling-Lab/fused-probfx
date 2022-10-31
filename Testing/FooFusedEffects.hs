{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Algebra (Has, send, Algebra (alg), type (:+:) (L), run)
import Control.Effect.Sum (type (:+:)(R))
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
    L AskFoo -> return $ (<$ ctx) 5
    R other  -> alg (runFoo . hdl) other ctx

action :: (Has Foo sig m) => m Int
action = do askFoo

main :: IO ()
main = do
  print $ run $ runFoo action


