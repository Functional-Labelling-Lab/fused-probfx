{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Carrier.Draw (
    DrawC,
    runDraw
) where

import           Control.Algebra              (Algebra (..), Handler, Has, run,
                                               send, type (:+:) (..))
import           Control.Carrier.State.Strict (StateC, evalState, runState)
import           Control.Effect.Dist          (Dist (..), dist)
import           Control.Effect.Draw          (Draw (..))
import qualified Control.Effect.State         as State
import           Data.Functor.Identity        (Identity)
import           Data.Kind                    (Type)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           PrimDist                     (Tag)

newtype DrawC m k = DrawC { runDrawC :: StateC (Int, Map.Map Tag Int) m k }
    deriving (Functor, Applicative, Monad)

runDraw :: Functor m => DrawC m a -> m a
runDraw = evalState (0, Map.empty) . runDrawC

instance (Algebra sig m, Has Dist sig m) => Algebra (Draw :+: sig) (DrawC m) where
  alg hdl sig ctx = DrawC $ case sig of
    L (Draw primDist mObs mTag) -> do
      -- Get current counter and tagMap
      (counter, tagMap) <- State.get @(Int, Map.Map Tag Int)

      -- Calculate tag and tagIdx to pass to the sample/observe
      let tag = fromMaybe (show counter) mTag
      let tagIdx = Map.findWithDefault 0 tag tagMap

      -- Increment the counter and the address
      let counter' = counter + 1
      let tagMap' = Map.insert tag (tagIdx + 1) tagMap
      State.put (counter', tagMap')

      x <- dist primDist mObs (tag, tagIdx)

      pure $ x <$ ctx

    R other -> alg (runDrawC . hdl) (R other) ctx
