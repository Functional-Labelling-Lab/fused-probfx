{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

import Prog (Member, Prog (Val, Op), discharge)

import Prog (Member, Prog (Val, Op), call, run)

data Foo k where
    AskFoo :: Foo Int

askFoo :: (Member Foo es) => Prog es Int
askFoo = call AskFoo

-- Handler
handleFoo :: Prog (Foo ': es) Int -> Prog es [Int]
handleFoo (Val n) = return [n]
handleFoo (Op es k) = case discharge es of
    Right AskFoo -> handleFoo (k 5)
    Left es' -> Op es' (handleFoo . k)

action :: (Member Foo es) => Prog es Int
action = do askFoo

main :: IO ()
main = do
  print $ run $ handleFoo action