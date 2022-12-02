{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
     This demonstrates:
      - The [SIR](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology) model for modelling
        the transition between Susceptible (S), Infected (I), and Recovered (R) individuals during an epidemic.
        We model this as a Hidden Markov Model, where the _latent states_ are the true values of S, I, and R,
        and the _observations_ are the reported number of infections (𝜉).
      - Extending the SIR to the SIRS model where recovered individuals (R) can become susceptible (S) again.
      - Extending the SIRS to the SIRSV model where susceptible individuals (S) can become vaccinated (V).

      Note that the extensions (SIRS and SIRSV) aren't as modular as we would like, due to having to
      redefine the data types Popl and TransParams when adding new variables to the SIR model.
      The file [SIRModular](examples/SIR.hs) shows how one could take steps to resolve this by using
      extensible records.
-}

module SIRNonModular where

import           Control.Algebra               (Has)
import           Control.Carrier.Writer.Strict (WriterC, runWriter, tell)
import           Control.Effect.Sum            ((:+:), Member)
import           Control.Effect.Writer         (Writer)
import           Control.Monad                 ((>=>))
import           Env                           (Assign ((:=)), Env, Observable,
                                                Observables, get, nil, (<:>))
import           HMM                           (ObsModel, TransModel, hmmGen)
import           Inference.MH                  as MH (mhRaw)
import           Inference.SIM                 as SIM (simulate)
import           Model                         (Model (Model, runModel), beta, binomial', gamma,
                                                poisson)
import           Sampler                       (Sampler)

runWriterM :: forall env w sig m a. Monoid w =>  Model env (Writer w :+: sig) (WriterC w m) a -> Model env sig m (w, a)
runWriterM model = Model $ runWriter $ runModel model

{- | SIR model.
-}

-- | SIR model environment
type SIRenv =
 '[ "β"  := Double  -- ^ mean contact rate between susceptible and infected people
  , "γ"  := Double  -- ^ mean recovery rate
  , "ρ"  := Double  -- ^ mean report rate of infection
  , "𝜉"  := Int     -- ^ number of reported infections
 ]

-- | Latent state
data Popl = Popl {
    s :: Int, -- ^ number of people susceptible to infection
    i :: Int, -- ^ number of people currently infected
    r :: Int  -- ^ number of people recovered from infection
} deriving Show

-- | Transition model parameters
data TransParamsSIR = TransParamsSIR {
    betaP  :: Double, -- ^ mean contact rate between susceptible and infected people
    gammaP :: Double  -- ^ mean recovery rate
}

-- | Observation 𝜉
type Reported = Int

-- | Observation model parameters
type ObsParams = Double

-- | Transition model prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env sig m TransParamsSIR
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (TransParamsSIR pBeta pGamma)

-- | Transition model between S and I
transSI :: TransModel env sig m Double Popl
transSI beta (Popl s i r) = do
  let pop = s + i + r
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ Popl (s - dN_SI) (i + dN_SI) r

-- | Transition model between I and R
transIR :: TransModel env sig m Double Popl
transIR gamma (Popl s i r)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ Popl s (i - dN_IR) (r + dN_IR)

-- | Transition model between S, I, and R
transSIR :: Member (Writer [Popl]) sig
  => TransModel env sig m TransParamsSIR Popl
transSIR (TransParamsSIR beta gamma) sir = do
  sir' <- (transSI beta >=> transIR gamma) sir
  Model $ tell [sir']  -- a user effect for writing each latent SIR state to a stream [Popl]
  return sir'

-- | Observation model prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env sig m ObsParams
obsPriorSIR = do
  pRho <- beta 2 7 #ρ
  return pRho

-- | Observation model from I to 𝜉
obsSIR :: Observable env "𝜉" Int
  => ObsModel env sig m Double Popl Reported
obsSIR rho (Popl _ i _)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIR as HMM
hmmSIR :: (Member (Writer [Popl]) sig, Observable env "𝜉" Int, Observables env '["ρ", "β", "γ"] Double)
  => Int -> Popl -> Model env sig m Popl
hmmSIR = hmmGen transPriorSIR obsPriorSIR transSIR obsSIR

-- | Handle the user effect for writing each SIR state to a stream [Popl]
hmmSIR' :: (Observables env '["𝜉"] Int , Observables env '[ "β" , "γ" , "ρ"] Double) => Int -> Popl -> Model env sig m ([Popl], Popl)
hmmSIR' n = runWriterM . hmmSIR n

-- | Simulating from SIR model: ([(s, i, r)], [𝜉])
simulateSIR :: Sampler ([(Int, Int, Int)], [Reported])
simulateSIR = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = Popl {s = 762, i = 1, r = 0}
  -- Specify model environment
      sim_env_in :: Env SIRenv
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
  -- Simulate an epidemic over 100 days
  ((sir_trace, _), sim_env_out) <- SIM.simulate sim_env_in $ hmmSIR' 100 sir_0
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIR values over 100 days
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)

-- | MH inference from SIR model: ([ρ], [β])
inferSIR :: Sampler ([Double], [Double])
inferSIR = do
  -- Simulate some observed infections
  𝜉s <- snd <$> simulateSIR
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0           = Popl {s = 762, i = 1, r = 0}
  -- Specify model environment
      mh_env_in :: Env SIRenv
      mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> nil
  -- Run MH inference over 50000 iterations
  mhTrace <- MH.mhRaw 5000 (hmmSIR' 100 sir_0) mh_env_in nil (#β <:> #ρ <:> nil)
  -- Get the sampled values for model parameters ρ and β
  let ρs = concatMap (get #ρ) mhTrace
      βs = concatMap (get #β) mhTrace
  return (ρs, βs)


{- | SIRS model.
-}
-- | SIRS model environment
type SIRSenv =
 '[ "β"  := Double  -- ^ mean contact rate between susceptible and infected people
  , "γ"  := Double  -- ^ mean recovery rate
  , "η"  := Double  -- ^ rate of resusceptible
  , "ρ"  := Double  -- ^ mean report rate of infection
  , "𝜉"  := Int     -- ^ number of reported infections
 ]

-- | Transition model parameters
data TransParamsSIRS = TransParamsSIRS {
    betaP_SIRS  :: Double, -- ^ mean contact rate between susceptible and infected people
    gammaP_SIRS :: Double, -- ^ mean recovery rate
    etaP_SIRS   :: Double  -- ^ rate of resusceptible
}

-- | Transition model prior
transPriorSIRS :: Observables env '["β", "η", "γ"] Double
  => Model env sig m TransParamsSIRS
transPriorSIRS = do
  TransParamsSIR pBeta pGamma  <- transPriorSIR
  pEta <- gamma 1 (1/8) #η
  return (TransParamsSIRS pBeta pGamma pEta)

-- | Transition model between R and S
transRS :: Double -> Popl -> Model env sig m Popl
transRS eta (Popl s i r) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ Popl (s + dN_RS) i (r - dN_RS)

-- | Transition model between S, to I, to R, and to S
transSIRS :: Member (Writer [Popl]) sig
  => TransModel env sig m TransParamsSIRS Popl
transSIRS (TransParamsSIRS beta gamma eta) sir = do
  sir' <- (transSI beta >=> transIR gamma >=> transRS eta) sir
  Model $ tell [sir']
  return sir'

-- | SIRS as HMM
hmmSIRS :: (Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double) => Int -> Popl -> Model env sig m ([Popl], Popl)
hmmSIRS n = runWriterM . hmmGen transPriorSIRS obsPriorSIR transSIRS obsSIR n

-- | Simulate from SIRS model: ([(s, i, r)], [𝜉])
simulateSIRS :: Sampler ([(Int, Int, Int)], [Reported])
simulateSIRS = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = Popl {s = 762, i = 1, r = 0}
  -- Specify model environment
      sim_env_in :: Env SIRSenv
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
  -- Simulate an epidemic over 100 days
  ((sir_trace, _), sim_env_out) <- SIM.simulate sim_env_in $ hmmSIRS 100 sir_0
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIR values over 100 days
      sirs = map (\(Popl s i recov) -> (s, i, recov)) sir_trace
  return (sirs, 𝜉s)


{- | SIRSV model.
-}
-- | SIRS model environment
type SIRSVenv =
 '[ "β"  := Double  -- ^ mean contact rate between susceptible and infected people
  , "γ"  := Double  -- ^ mean recovery rate
  , "η"  := Double  -- ^ rate of resusceptible
  , "ω"  := Double  -- ^ vaccination rate
  , "ρ"  := Double  -- ^ mean report rate of infection
  , "𝜉"  := Int     -- ^ number of reported infections
 ]

-- | Transition model parameters
data TransParamsSIRSV = TransParamsSIRSV {
    betaP_SIRSV  :: Double, -- ^ mean contact rate between susceptible and infected people
    gammaP_SIRSV :: Double, -- ^ mean recovery rate
    etaP_SIRSV   :: Double, -- ^ rate of resusceptible
    omegaP_SIRSV :: Double  -- ^ vaccination rate
}

-- | Latent state
data PoplV = PoplV {
    s' :: Int,  -- ^ susceptible individuals
    i' :: Int,  -- ^ infected individuals
    r' :: Int,  -- ^ recovered individuals
    v' :: Int   -- ^ vaccinated individuals
} deriving Show

-- | Transition from S to I
transSI' :: TransModel env sig m Double PoplV
transSI' beta (PoplV s i r v) = do
  let pop = s + i + r + v
  dN_SI <- binomial' s (1 - exp ((-beta * fromIntegral i) / fromIntegral pop))
  return $ PoplV (s - dN_SI) (i + dN_SI) r v

-- | Transition from I to R
transIR' :: TransModel env sig m Double PoplV
transIR' gamma (PoplV s i r v)  = do
  dN_IR <- binomial' i (1 - exp (-gamma))
  return $ PoplV s (i - dN_IR) (r + dN_IR) v

-- | Transition from R to S
transRS' :: TransModel env sig m Double PoplV
transRS' eta (PoplV s i r v) = do
  dN_RS <- binomial' r (1 - exp (-eta))
  return $ PoplV (s + dN_RS) i (r - dN_RS) v

-- | Transition from S to V
transSV' :: TransModel env sig m Double PoplV
transSV' omega (PoplV s i r v)  = do
  dN_SV <- binomial' s (1 - exp (-omega))
  return $  PoplV (s - dN_SV) i r (v + dN_SV )

-- | Transition between S to I, I to R, R to S, and S to V
transSIRSV :: Member (Writer [PoplV]) sig => TransModel env sig m TransParamsSIRSV PoplV
transSIRSV (TransParamsSIRSV beta gamma omega eta) sirv = do
  sirv' <- (transSI' beta  >=>
            transIR' gamma >=>
            transRS' eta   >=>
            transSV' omega) sirv
  Model $ tell [sirv']
  return sirv'

-- | Transition model prior
transPriorSIRSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env sig m TransParamsSIRSV
transPriorSIRSV  = do
  TransParamsSIRS pBeta pGamma pEta <- transPriorSIRS
  pOmega <- gamma 1 (1/16) #ω
  return (TransParamsSIRSV pBeta pGamma pEta pOmega)

-- | Observation model
obsSIRSV :: Observable env "𝜉" Int
  => ObsModel env sig m Double PoplV Reported
obsSIRSV rho (PoplV _ i _ v)  = do
  i <- poisson (rho * fromIntegral i) #𝜉
  return i

-- | SIRSV as HMM
hmmSIRSV ::  (Observables env '["𝜉"] Int, Observables env '["β", "γ", "η", "ω", "ρ"] Double) => Int -> PoplV -> Model env sig m ([PoplV], PoplV)
hmmSIRSV n = runWriterM . hmmGen transPriorSIRSV obsPriorSIR transSIRSV obsSIRSV n

-- | Simulate from SIRSV model : ([(s, i, r, v)], [𝜉])
simulateSIRSV :: Sampler ([(Int, Int, Int, Int)], [Reported])
simulateSIRSV = do
  -- Specify model input of 762 susceptible and 1 infected
  let sirv_0      = PoplV {s' = 762, i' = 1, r' = 0, v' = 0}
  -- Specify model environment
      sim_env_in :: Env SIRSVenv
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ω := [0.02] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
  -- Simulate an epidemic over 100 days
  ((sirv_trace, _), sim_env_out) <- SIM.simulate sim_env_in $ hmmSIRSV 100 sirv_0
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIRV values over 100 days
      sirvs = map (\(PoplV s i recov v) -> (s, i, recov, v)) sirv_trace
  return (sirvs, 𝜉s)
