{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{- | Metropolis-Hastings inference.
-}

module Inference.MH (
    Transitions
  , mh
  , mhRaw
  , mhStep
  , runMH
  , lookupSample
  , accept) where

import           Control.Algebra               (Algebra (alg), Has)
import           Control.Carrier.Dist          (DistC, runDist)
import           Control.Carrier.Lift          (Lift, LiftC, runM, sendM)
import           Control.Carrier.LPTracer      (LPTracerC, runLPTracer)
import           Control.Carrier.ObsReader     (ObsReaderC, runObsReader)
import           Control.Carrier.Reader        (ReaderC, ask, runReader)
import           Control.Carrier.SampTracer    (SampTracerC, runSampTracer)
import           Control.Effect.Dist           (Dist)
import           Control.Effect.ObsReader      (ObsReader)
import           Control.Effect.SampObs        (SampObs (..))
import           Control.Effect.Sum            ((:+:) (L, R))
import           Control.Monad                 ((>=>))
import           Control.Monad.Trans           (MonadTrans (lift))
import           Data.Kind                     (Type)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromJust, fromMaybe)
import           Data.Set                      (Set, (\\))
import qualified Data.Set                      as Set
import qualified Data.WorldPeace               as WP
import qualified Data.WorldPeace.Extra         as WPE
import           Data.WorldPeace.Product.Extra (DefaultProduct (..), pfoldr)
import           Env                           (Assign (..),
                                                ConstructProduct (..), Env,
                                                ExtractTypes, ExtractVars,
                                                HasObsVar, ObsVar, nil,
                                                varToStr)
import           GHC.Types                     (Symbol)
import           Model                         (Model, runModel)
import           PrimDist                      (Addr,
                                                ErasedPrimDist (ErasedPrimDist),
                                                PrimDist (DiscrUniformDist, UniformDist),
                                                PrimVal, Tag, dist,
                                                pattern PrimDistPrf)
import           Sampler                       (Sampler, liftS)
import           Trace                         (FromSTrace (..), LPTrace,
                                                STrace, updateLPTrace)
import           Unsafe.Coerce                 (unsafeCoerce)

-- | Carrier for the 'Control.Effect.SampObs' effect for simulating the
--   transition models
newtype SampObsTransC (m :: * -> *) (k :: *) = SampObsTransC { runSampObsTrans :: m k }
    deriving (Functor, Applicative, Monad)

instance (Has (Lift Sampler) sig m) => Algebra (SampObs :+: sig) (SampObsTransC m) where
  alg hdl sig ctx = SampObsTransC $ case sig of
    L (SampObs d Nothing addr) -> do
      x <- sendM $ dist d
      pure $ x <$ ctx
    L _ -> error "Should not have used the observe since the environment is empty"
    R other -> alg (runSampObsTrans . hdl) other ctx

-- | Carrier for the 'Control.Effect.SampObs' effect for running MH on the
--   primary model
newtype SampObsC (m :: * -> *) (k :: *) = SampObsC { runSampObs :: ReaderC (TransitionMap, STrace, Addr) m k }
    deriving (Functor, Applicative, Monad)

instance (Has (Lift Sampler) sig m) => Algebra (SampObs :+: sig) (SampObsC m) where
  alg hdl sig ctx = SampObsC $ case sig of
    L (SampObs (PrimDistPrf d) obs addr) -> case obs of
      Just y  -> pure $ y <$ ctx
      Nothing -> do
        (trans, strace, α_samp) <- ask @(TransitionMap, STrace, Addr)
        x <- sendM $ lookupSample trans strace d addr α_samp
        pure $ x <$ ctx
    R other -> alg (runSampObs . hdl) (R other) ctx

-- | Product interpretation for optional sample sites
newtype ObsSite x = ObsSite (ObsVar x)

instance ConstructProduct ObsSite x sampled (ObsVar x) where
  constructProduct = ObsSite

-- | Optional sites for which variables to resample during MH
type ObsSites env = WP.Product ObsSite env

-- | Product interpretation for variable transition models
data Transition (sig :: (* -> *) -> * -> *) (m :: * -> *) (e :: Assign Symbol *) where
  Transition :: ObsVar x -> (a -> Model '[] sig m a) -> Transition sig m (x ':= a)

instance HasObsVar x trans ~ False => ConstructProduct (Transition sig m) (x := a) trans (Assign (ObsVar x) (a -> Model '[] sig m a)) where
  constructProduct (x := trans) = Transition x trans

-- | Transition models to resample variables with during execution; if a model
--   is not specified the variable will be resampled using the
--   original distribution
type Transitions env = WP.Product (Transition (MhSig '[]) MhTransCarrier) env

-- | Transition model entry with the transition type removed from the signature
data ErasedTransition sig m where
  ErasedTransition :: (a -> Model '[] sig m a) -> ErasedTransition sig m

-- | Map from dynamic values of variables to their transition model
type TransitionMap = Map.Map Tag (ErasedTransition (MhSig '[]) MhTransCarrier)

-- Effect and carrier for the primary model
type MhSig env = ObsReader env :+: Dist :+: SampObs :+: Lift Sampler
type MhCarrier env = ObsReaderC env (DistC (SampTracerC (LPTracerC (SampObsC (LiftC Sampler)))))

-- Effect and carrier for the transition models
type MhTransSig = MhSig '[]
type MhTransCarrier =  ObsReaderC '[] (DistC (SampObsTransC (LiftC Sampler)))

-- | Top-level wrapper for Metropolis-Hastings (MH) inference - successful sampling indexing version
mh :: forall env trans a sampled. (FromSTrace env, WP.Contains trans env, WP.Contains sampled (ExtractVars env))
  => Int -- number of MH samplings
  -> Model env (MhSig env) (MhCarrier env) a -- ^ model awaiting an input
  -> Env env           -- ^ (model input, input model environment)
  -> Transitions trans -- ^ Optional explicit transition models
  -> ObsSites sampled  -- ^ optional list of observable variable names (strings) to specify sample sites of interest
           {- For example, provide "mu" to specify interest in sampling #mu. This causes other variables to not be resampled unless necessary. -}
  -> Sampler [Env env] -- ^ [output model environment]
mh n model env_0 trans tagsP = do
  -- Perform initial run of MH with no proposal sample site
  y0 <- runMH model env_0 Map.empty Map.empty ("", 0)
  -- Perform n MH iterations
  let tags = pfoldr (\(ObsSite x) tags -> varToStr x : tags) [] tagsP
  let transMap = pfoldr (\(Transition x trans) -> Map.insert (varToStr x) (ErasedTransition trans)) Map.empty trans
  mhTrace <- loop n [y0] (mhStep model env_0 transMap tags)
  -- Return sample trace
  return $ map (\((_, strace), _) -> fromSTrace strace) mhTrace
  where
    loop :: Monad m => Int -> [a'] -> (a' -> m (Maybe a')) -> m [a']
    loop 0 xs _ = return xs
    loop n xs@(x : _) step = do
      maybeNewX <- step x
      maybe (loop n xs step) (\newX -> loop (n - 1) (newX : xs) step) maybeNewX

-- | Top-level wrapper for Metropolis-Hastings (MH) inference - raw indexing version
mhRaw :: forall env trans a sampled. (FromSTrace env, WP.Contains trans env, WP.Contains sampled (ExtractVars env))
  => Int -- number of MH samplings
  -> Model env (MhSig env) (MhCarrier env) a -- ^ model awaiting an input
  -> Env env           -- ^ (model input, input model environment)
  -> Transitions trans -- ^ optional explicit transition models
  -> ObsSites sampled  -- ^ optional list of observable variable names (strings) to specify sample sites of interest
           {- For example, provide "mu" to specify interest in sampling #mu. This causes other variables to not be resampled unless necessary. -}
  -> Sampler [Env env] -- ^ [output model environment]
mhRaw n model env_0 trans tagsP = do
  -- Perform initial run of MH with no proposal sample site
  y0 <- runMH model env_0 Map.empty Map.empty ("", 0)
  -- Perform n MH iterations
  let tags = pfoldr (\(ObsSite x) tags -> varToStr x : tags) [] tagsP
  let transMap = pfoldr (\(Transition x trans) -> Map.insert (varToStr x) (ErasedTransition trans)) Map.empty trans
  mhTrace <- loop n [y0] (mhStep model env_0 transMap tags)
  -- Return sample trace
  return $ map (\((_, strace), _) -> fromSTrace strace) mhTrace
  where
    loop :: Monad m => Int -> [a'] -> (a' -> m (Maybe a')) -> m [a']
    loop 0 xs _ = return xs
    loop n xs@(x : _) step = do
      maybeNewX <- step x
      loop (n - 1) (maybe xs (:xs) maybeNewX) step

-- | Perform one step of MH
mhStep ::
     Model env (MhSig env) (MhCarrier env) a -- ^ model
  -> Env env       -- ^ model environment
  -> TransitionMap -- ^ optional explicit transition models
  -> [Tag]         -- ^ optional list of observable variable names (strings) to specify sample sites of interest
  -> ((a, STrace), LPTrace)                 -- ^ trace of previous MH outputs
  -> Sampler (Maybe ((a, STrace), LPTrace)) -- ^ updated trace of MH outputs
mhStep model env tranMaps tags trace = do
  -- Get previous mh output
  let ((x, samples), logps) = trace
  -- Get possible addresses to propose new samples for
      sampleSites = if null tags then samples
                      else Map.filterWithKey (\(tag, i) _ -> tag `elem` tags) samples
  -- Dist a proposal sample address
  α_samp_ind <- dist $ DiscrUniformDist 0 (Map.size sampleSites - 1)
  let (α_samp, _) = Map.elemAt α_samp_ind sampleSites
  -- Run MH with proposal sample address
  newTrace@((_, samples'), logps') <- runMH model env tranMaps samples α_samp
  -- Compute acceptance ratio
  acceptance_ratio <- liftS $ accept α_samp samples samples' logps logps'
  u <- dist (UniformDist 0 1)
  if u < acceptance_ratio
    then return $ Just newTrace
    else return Nothing

-- | Handler for one iteration of MH
runMH ::
     Model env (MhSig env) (MhCarrier env) a -- ^ model
  -> Env env        -- ^ model environment
  -> TransitionMap  -- ^ optional explicit transition models
  -> STrace         -- ^ sample trace of previous MH iteration
  -> Addr           -- ^ sample address of interest
  -> Sampler ((a, STrace), LPTrace) -- ^ (model output, sample trace, log-probability trace)
runMH model env transMap strace α_samp =
     runM $ runReader (transMap, strace, α_samp)
          $ runSampObs $ runLPTracer True $ runSampTracer $ runDist
          $ runObsReader env $ runModel model

runTransModel :: Model '[] (MhSig '[]) MhTransCarrier a -> Sampler a -- ^ model
runTransModel model = runM $ runSampObsTrans $ runDist $ runObsReader nil $ runModel model

-- | For a given address, look up a sampled value from a sample trace, returning
--   it only if the primitive distribution it was sampled from matches the current one.
lookupSample :: forall a. WPE.IsMember a PrimVal
  => TransitionMap
  -> STrace     -- ^ sample trace
  -> PrimDist a -- ^ distribution to sample from
  -> Addr       -- ^ address of current sample site
  -> Addr       -- ^ address of proposal sample site
  -> Sampler a
lookupSample transMap samples d α@(t, _) α_samp
  | α == α_samp = fromMaybe (dist d) $ do
      (_, x) <- Map.lookup α samples
      (ErasedTransition trans) <- Map.lookup t transMap
      return $ runTransModel $ unsafeCoerce trans $ fromJust $ WPE.openUnionMatch @a x
  | otherwise = fromMaybe (dist d) $ do
        (ErasedPrimDist d', x) <- Map.lookup α samples
        return $
          if d == unsafeCoerce d'
            then return (fromJust $ WPE.openUnionMatch x)
          else dist d

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
