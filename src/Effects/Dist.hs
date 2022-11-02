{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

{- | The effects for primitive distributions, sampling, and observing.
-}

module Effects.Dist (
  -- ** Address
  -- $Address
    Tag
  , Addr
  -- ** Dist effect
  , Dist(..)
  -- ** Sample effect
  , Sample(..)
  , pattern Samp
  -- ** Observe effect
  , Observe(..)
  , pattern Obs
  , handleDist
  , handleDist
  ) where

import Data.Map (Map)
import Data.Maybe ( fromMaybe )
import Prog ( call, discharge, Member(..), Prog(..), EffectSum(..) )
import qualified Data.Map as Map
import PrimDist ( PrimDist )
import Data.Kind (Type)
import Control.Algebra (type (:+:) (L, R), Algebra(..), Has, send)
import qualified Control.Algebra as Control.Algebra.Handler

{- $Address
   Run-time identifiers for probabilistic operations
-}

-- | An observable variable name assigned to a primitive distribution, representing a compile-time identifier
type Tag  = String
-- | An observable variable name and the index of its run-time occurrence, representing a run-time identifier
type Addr = (Tag, Int)

-- | The effect @Dist@ for primitive distributions
data Dist (a :: Type) (m :: Type -> Type) (k :: Type) = Dist
  { getPrimDist :: PrimDist a  -- ^ primitive distribution
  , getObs :: Maybe a          -- ^ optional observed value
  , getTag :: Maybe Tag        -- ^ optional observable variable name
  }

instance Show a => Show (Dist a m k) where
  show (Dist d y tag) = "Dist(" ++ show d ++ ", " ++ show y ++ ", " ++ show tag ++ ")"

instance Eq (Dist a m k) where
  (==) (Dist d1 _ _) (Dist d2 _ _) = d1 == d2

-- | The effect @Sample@ for sampling from distribution
data Sample a where
  Sample  :: PrimDist a     -- ^ distribution to sample from
          -> Addr           -- ^ address of @Sample@ operation
          -> Sample a

-- | For projecting and then successfully pattern matching against @Sample@
pattern Samp :: Member Sample es => PrimDist x -> Addr -> EffectSum es x
pattern Samp d α <- (prj  -> Just (Sample d α))

-- | The effect @Observe@ for conditioning against observed values
data Observe a where
  Observe :: PrimDist a     -- ^ distribution to condition with
          -> a              -- ^ observed value
          -> Addr           -- ^ address of @Observe@ operation
          -> Observe a

-- | For projecting and then successfully pattern matching against @Observe@
pattern Obs :: Member Observe es => PrimDist x -> x -> Addr -> EffectSum es x
pattern Obs d y α <- (prj -> Just (Observe d y α))

-- | Handle the @Dist@ effect to a @Sample@ or @Observe@ effect and assign an address

newtype DistC a m k = DistC {runDist :: m k}
  deriving (Applicative, Functor, Monad)

-- instance Algebra sig m => Algebra (Dist a :+: sig) (DistC a m) where
--   alg :: (Algebra sig m, Functor ctx)
--       => Control.Algebra.Handler.Handler ctx n (DistC a m)
--       -> (Dist a :+: sig) n a1
--       -> ctx ()
--       -> DistC a m (ctx a1)
--   alg = loop 0 Map.empty
--     where
--       loop :: (Algebra sig m, Functor ctx)
--         => Int
--         -> Map Tag Int
--         -> Control.Algebra.Handler.Handler ctx n (DistC a m)
--         -> (Dist a :+: sig) n a1
--         -> ctx ()
--         -> DistC a m (ctx a1)
--       loop counter tagMap hdl sig ctx = DistC $ case sig of
--         L (Dist primDist mObs mTag) -> case mObs of
--                   Just obs  -> do 
--                     x <- send (observe primDist obs (tag, tagIdx))
--                     pure $ x <$ ctx'
--                   Nothing -> do 
--                     x <- send (sample primDist (tag, tagIdx))
--                     pure $ x <$ ctx'
--               where 
--                     tag     = fromMaybe (show counter) mTag
--                     tagIdx  = Map.findWithDefault 0 tag tagMap
--                     tagMap' = Map.insert tag (tagIdx + 1) tagMap

--                     -- ctx' = loop (counter + 1) tagMap' hdl sig' ctx 
--                     -- ctx' :: a
--                     rf = loop (counter + 1) tagMap' hdl sig

--                     ctx' = rf <$> ctx

--                     observe = undefined
--                     sample = undefined
--         R  other  -> alg (runDist . hdl) other ctx

handleDist = undefined
