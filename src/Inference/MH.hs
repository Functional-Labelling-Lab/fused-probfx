{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Metropolis-Hastings inference.
-}

module Inference.MH (
    mh
  , mhStep
  , runMH
  , lookupSample
  , accept) where

import           Control.Algebra              (Algebra (alg))
import           Control.Carrier.Draw         (DrawC, runDraw)
import           Control.Carrier.Lift         (Lift, LiftC, runM, sendM)
import           Control.Carrier.LPTracer     (LPTracerC, runLPTracer)
import           Control.Carrier.ObsReader    (ObsReaderC, runObsReader)
import           Control.Carrier.Reader       (ReaderC, ask, runReader)
import           Control.Carrier.SampleTracer (SampleTracerC, runSampleTracer)
import           Control.Effect.Dist          (Dist (..))
import           Control.Monad                ((>=>))
import           Control.Monad.Trans          (MonadTrans (lift))
import           Data.Kind                    (Type)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import           Data.Set                     (Set, (\\))
import qualified Data.Set                     as Set
import           Env                          (Env)
import           OpenSum                      (OpenSum (..))
import qualified OpenSum
import           PrimDist                     (Addr,
                                               ErasedPrimDist (ErasedPrimDist),
                                               PrimDist (DiscrUniformDist, UniformDist),
                                               PrimVal, Tag, draw,
                                               pattern PrimDistPrf)
import           Sampler                      (Sampler, liftS)
import           Trace                        (FromSTrace (..), LPTrace, STrace,
                                               updateLPTrace)
import           Unsafe.Coerce                (unsafeCoerce)
import Control.Effect.Sum ((:+:)(L, R))
import Control.Algebra (Has)

-- Dist Effect Carrier
newtype DistC (m :: * -> *) (k :: *) = DistC { runDist :: ReaderC (STrace, Addr) m k }
    deriving (Functor, Applicative, Monad)

instance (Has (Lift Sampler) sig m) => Algebra (Dist :+: sig) (DistC m) where
  alg hdl sig ctx = DistC $ case sig of
    L (Dist (PrimDistPrf d) obs addr) -> case obs of
      Just y  -> pure $ y <$ ctx
      Nothing -> do
        (strace, α_samp) <- ask @(STrace, Addr)
        x <- sendM $ lookupSample strace d addr α_samp
        pure $ x <$ ctx
    R other -> alg (runDist . hdl) (R other) ctx

-- | Top-level wrapper for Metropolis-Hastings (MH) inference
mh :: (FromSTrace env)
  => Int            -- ^ number of MH iterations
  -> ObsReaderC env (DrawC (SampleTracerC (LPTracerC (DistC (LiftC Sampler))))) a -- ^ model awaiting an input
  -> Env env        -- ^ (model input, input model environment)
  -> [Tag] -- ^ optional list of observable variable names (strings) to specify sample sites of interest
           {- For example, provide "mu" to specify interest in sampling #mu. This causes other variables to not be resampled unless necessary. -}
  -> Sampler [Env env] -- ^ [output model environment]
mh n model env_0 tags = do
  -- Perform initial run of MH with no proposal sample site
  y0 <- runMH model env_0 Map.empty ("", 0)
  -- Perform n MH iterations
  mhTrace <- foldl (>=>) return (replicate n (mhStep model env_0 tags)) [y0]
  -- Return sample trace
  return $ map (\((_, strace), _) -> fromSTrace strace) mhTrace

-- | Perform one step of MH
mhStep ::
     ObsReaderC env (DrawC (SampleTracerC (LPTracerC (DistC (LiftC Sampler))))) a -- ^ model
  -> Env env                  -- ^ model environment
  -> [Tag]                    -- ^ tags indicating sample sites of interest
  -> [((a, STrace), LPTrace)] -- ^ trace of previous MH outputs
  -> Sampler [((a, STrace), LPTrace)] -- ^ updated trace of MH outputs
mhStep model env tags trace = do
  -- Get previous mh output
  let ((x, samples), logps) = head trace
  -- Get possible addresses to propose new samples for
      sampleSites = if null tags then samples
                    else  Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  -- Draw a proposal sample address
  α_samp_ind <- draw $ DiscrUniformDist 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- Run MH with proposal sample address
  ((x', samples'), logps') <- runMH model env samples α_samp
  -- Compute acceptance ratio
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- draw (UniformDist 0 1)
  if u < acceptance_ratio
    then do return (((x', samples'), logps'):trace)
    else do return trace

-- | Handler for one iteration of MH
runMH ::
     ObsReaderC env (DrawC (SampleTracerC (LPTracerC (DistC (LiftC Sampler))))) a -- ^ model
  -> Env env        -- ^ model environment
  -> STrace         -- ^ sample trace of previous MH iteration
  -> Addr           -- ^ sample address of interest
  -> Sampler ((a, STrace), LPTrace) -- ^ (model output, sample trace, log-probability trace)
runMH model env strace α_samp =
     runM $ runReader (strace, α_samp) $ runDist $ runLPTracer True $ runSampleTracer $ runDraw $ runObsReader env model

-- | Handler for @Sample@ that selectively reuses old samples or draws new ones
-- handleSamp ::
--      STrace -- ^ sample trace
--   -> Addr   -- ^ address of the proposal sample site for the current MH iteration
--   -> Prog '[Sample] a
--   -> Sampler a
-- handleSamp strace α_samp (Op op k) = case discharge op of
--   Right (Sample (PrimDistPrf d) α) ->
--         do x <- lookupSample strace d α α_samp
--            handleSamp strace α_samp (k x)
--   _  -> error "Impossible: Nothing cannot occur"
-- handleSamp _ _ (Val x) = return x

-- | For a given address, look up a sampled value from a sample trace, returning
--   it only if the primitive distribution it was sampled from matches the current one.
lookupSample :: OpenSum.Member a PrimVal
  =>
     STrace     -- ^ sample trace
  -> PrimDist a -- ^ distribution to sample from
  -> Addr       -- ^ address of current sample site
  -> Addr       -- ^ address of proposal sample site
  -> Sampler a
lookupSample samples d α α_samp
  | α == α_samp = draw d
  | otherwise   =
      case Map.lookup α samples of
        Just (ErasedPrimDist d', x) -> do
          if d == unsafeCoerce d'
            then return (fromJust $ OpenSum.prj x)
            else draw d
        Nothing -> draw d

-- | Compute acceptance probability
accept :: Addr -- ^ address of new sampled value
  -> STrace    -- ^ previous MH sample trace
  -> STrace    -- ^ new MH sample trace
  -> LPTrace   -- ^ previous MH log-probability trace
  -> LPTrace   -- ^ current MH log-probability trace
  -> IO Double
accept x0 _Ⲭ _Ⲭ' logℙ logℙ' = do
  let _X'sampled = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ' \\ Map.keysSet _Ⲭ)
      _Xsampled  = Set.singleton x0 `Set.union` (Map.keysSet _Ⲭ \\ Map.keysSet _Ⲭ')
  let dom_logα   = log (fromIntegral $ Map.size _Ⲭ) - log (fromIntegral $ Map.size _Ⲭ')
  let _Xlogα     = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ))
                         0 (Map.keysSet logℙ \\ _Xsampled)
  let _X'logα    = foldl (\logα v -> logα + fromJust (Map.lookup v logℙ'))
                         0 (Map.keysSet logℙ' \\ _X'sampled)
  return $ exp (dom_logα + _X'logα - _Xlogα)
