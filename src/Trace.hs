{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- | For recording samples and log-probabilities during model execution.
-}

module Trace (
  -- * Sample trace
    STrace
  , FromSTrace(..)
  , updateSTrace
  -- * Log-probability trace
  , LPTrace
  , updateLPTrace) where

import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy (..))
import           Data.WorldPeace       (Product (Cons))
import qualified Data.WorldPeace       as WP
import qualified Data.WorldPeace.Extra as WPE
import           Env                   (Assign ((:=)), Env, EnvElem (..),
                                        HasObsVar, ObsVar (..), nil, varToStr)
import           GHC.TypeLits          (KnownSymbol)
import           PrimDist              (Addr, ErasedPrimDist (..), PrimDist,
                                        PrimVal, logProb)

{- | The type of sample traces, mapping addresses of sample/observe operations
     to their primitive distributions and sampled values.
-}
type STrace = Map Addr (ErasedPrimDist, WP.OpenUnion PrimVal)

-- | For converting sample traces to model environments
class FromSTrace env where
  -- | Convert a sample trace to a model environment
  fromSTrace :: STrace -> Env env

instance FromSTrace '[] where
  fromSTrace _ = nil

instance (HasObsVar x env ~ False, KnownSymbol x, Eq a, WPE.IsMember a PrimVal, FromSTrace env) => FromSTrace ((x := a) : env) where
  fromSTrace sMap = Cons (Elem $ extractSamples (ObsVar @x, Proxy @a) sMap) (fromSTrace sMap)

extractSamples ::  forall a x. (Eq a, WPE.IsMember a PrimVal) => (ObsVar x, Proxy a) -> STrace -> [a]
extractSamples (x, typ)  =
    map (fromJust . WPE.openUnionMatch @a . snd . snd)
  . Map.toList
  . Map.filterWithKey (\(tag, _) _ -> tag == varToStr x)

-- | Update a sample trace at an address
updateSTrace :: (Show x, WPE.IsMember x PrimVal) =>
     Addr       -- ^ address of sample site
  -> PrimDist x -- ^ primitive distribution at address
  -> x          -- ^ sampled value
  -> STrace     -- ^ previous sample trace
  -> STrace     -- ^ updated sample trace
updateSTrace α d x = Map.insert α (ErasedPrimDist d, WPE.openUnionLift x)

{- | The type of log-probability traces, mapping addresses of sample/observe operations
     to their log probabilities
-}
type LPTrace = Map Addr Double

-- | Compute and update a log-probability trace at an address
updateLPTrace ::
     Addr       -- ^ address of sample/observe site
  -> PrimDist x -- ^ primitive distribution at address
  -> x          -- ^ sampled or observed value
  -> LPTrace    -- ^ previous log-prob trace
  -> LPTrace    -- ^ updated log-prob trace
updateLPTrace α d x = Map.insert α (logProb d x)
