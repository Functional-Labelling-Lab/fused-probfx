{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{- | State effect.
-}

module Effects.State (
    State(..)
  , get
  , put
  , modify
  , handleState) where

import           Prog (Member (inj), Prog (..), discharge)

-- | The state effect
data State s a where
  -- | Get the current state
  Get :: State s s
  -- | Set the current state
  Put :: s -> State s ()

-- | Wrapper function for @Get@
get :: Member (State s) es => Prog es s
get = Op (inj Get) Val

-- | Wrapper function for @Put@
put :: (Member (State s) es) => s -> Prog es ()
put s = Op (inj $ Put s) Val

-- | Wrapper function for apply a function to the state
modify :: Member (State s) es => (s -> s) -> Prog es ()
modify f = get >>= put . f

-- | Handle the @State s@ effect
handleState :: s -> Prog (State s ': es) a -> Prog es (a, s)
handleState s (Val x) = return (x, s)
handleState s (Op u k) = case discharge u of
  Right Get      -> handleState s (k s)
  Right (Put s') -> handleState s' (k ())
  Left  u'       -> Op u' (handleState s . k)
