{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Prog (Member, Prog (Val, Op), call, run, discharge)
import Effects.ObsReader (ObsReader, handleRead, ask)
import Env (Observable, Assign ((:=)), (<:>), nil, Env)
import GHC.Base (Symbol)

data Foo k where
    AskFoo :: Foo Int

askFoo :: (Member Foo es) => Prog es Int
askFoo = call AskFoo

-- Handler
handleFoo :: Prog (Foo ': es) a -> Prog es a
handleFoo (Val n) = pure n
handleFoo (Op es k) = case discharge es of
    Right AskFoo -> handleFoo (k 5)
    Left es' -> Op es' (handleFoo . k)


action :: (Member Foo es) => Prog es Int
action = do
    x <- askFoo
    y <- askFoo
    return x


main = do run (handleFoo action :: Prog '[] Int)


-------


action' :: forall env es. (Member (ObsReader env) es, Observable env "x" Int) => Prog es [Maybe Int]
action' = do
    x <- ask @env #x
    y <- ask @env #x
    return [x, y]

main :: IO ()
main = do
    let env = (#x := [1, 2]) <:> nil
    print $ run $ handleRead env action'
--   print $ run $ handleFoo action