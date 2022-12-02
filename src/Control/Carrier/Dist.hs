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

module Control.Carrier.Dist (
    DistC,
    runDist
) where

import           Control.Algebra              (Algebra (..), Handler, Has, run,
                                               send, type (:+:) (..))
import           Control.Carrier.State.Strict (StateC, evalState, runState)
import           Control.Effect.Dist          (Dist (..))
import           Control.Effect.SampObs       (SampObs (..), sampObs)
import qualified Control.Effect.State         as State
import           Data.Functor.Identity        (Identity)
import           Data.Kind                    (Type)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           PrimDist                     (Tag)

newtype DistC m k = DistC { runDistC :: StateC (Int, Map.Map Tag Int) m k }
    deriving (Functor, Applicative, Monad)

runDist :: Functor m => DistC m a -> m a
runDist = evalState (0, Map.empty) . runDistC

instance Has SampObs sig m => Algebra (Dist :+: sig) (DistC m) where
  alg hdl sig ctx = DistC $ case sig of
    L (Dist primDist mObs mTag) -> do
      -- Get current counter and tagMap
      (counter, tagMap) <- State.get @(Int, Map.Map Tag Int)

      -- Calculate tag and tagIdx to pass to the sample/observe
      let tag = fromMaybe (show counter) mTag
      let tagIdx = Map.findWithDefault 0 tag tagMap

      -- Increment the counter and the address
      let counter' = counter + 1
      let tagMap' = Map.insert tag (tagIdx + 1) tagMap
      State.put (counter', tagMap')

      x <- sampObs primDist mObs (tag, tagIdx)

      pure $ x <$ ctx

    R other -> alg (runDistC . hdl) (R other) ctx
