{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

{- | This demonstrates:
      - The [SIR](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology) model for modelling
        the transition between Susceptible (S), Infected (I), and Recovered (R) individuals during an epidemic.
        We model this as a Hidden Markov Model, where the latent states are the true values of S, I, and R,
        and the observations are the reported number of infections (𝜉).
      - Extending the SIR to the SIRS model where recovered individuals (R) can become susceptible (S) again.
      - Extending the SIRS to the SIRSV model where susceptible individuals (S) can become vaccinated (V).

    For convenience, this makes use of the 'Data.Extensible' library for extensible records, and the 'Control.Lens'
    library to record accessors. If the lens notation is unfamiliar, the code below can be corresponded to a less modular
    version the file [SIRNonModular](examples/SIRNonModular.hs).
 -}

module SIR where

import           Control.Algebra               (Has)
import           Control.Carrier.Writer.Strict (WriterC, runWriter, tell)
import           Control.Effect.Sum            ((:+:))
import           Control.Effect.Writer         (Writer)
import           Control.Lens                  ((&), (.~), (^.))
import           Control.Monad                 ((>=>))
import           Data.Extensible               (Assoc ((:>)), Lookup, Record,
                                                emptyRecord, mkField, type (>:),
                                                (<:), (@=))
import           Data.Kind                     (Constraint)
import           Env                           (Assign ((:=)), Env, Observable,
                                                Observables, get, nil, (<:>))
import           GHC.TypeLits                  (Symbol)
import           HMM                           (ObsModel, TransModel, hmmGen)
import           Inference.MH                  as MH (mhRaw)
import           Inference.SIM                 as SIM (simulate)
import           Model                         (Model (..), beta, binomial',
                                                gamma, poisson)
import           Sampler                       (Sampler)
import Control.Effect.Sum (Member)

runWriterM :: forall env w sig m a. Monoid w =>  Model env (Writer w :+: sig) (WriterC w m) a -> Model env sig m (w, a)
runWriterM model = Model $ runWriter $ runModel @env model

-- | A type family for conveniently specifying multiple @Record@ fields of the same type
type family Lookups env (ks :: [Symbol]) a :: Constraint where
  Lookups env (x ': xs) a = (Lookup env x a, Lookups env xs a)
  Lookups env '[] a = ()

-- | HMM latent states (SIRV)
mkField "s" -- ^ susceptible individuals
mkField "i" -- ^ infected individuals
mkField "r" -- ^ recovered individuals
mkField "v" -- ^ vaccinated individuals

-- | HMM observations (𝜉) i.e. reported infections
type Reported = Int

{- | SIR model.
-}

-- | SIR model environment
type SIRenv =
 '[ "β"  := Double  -- ^ mean contact rate between susceptible and infected people
  , "γ"  := Double  -- ^ mean recovery rate
  , "ρ"  := Double  -- ^ mean report rate of infection
  , "𝜉"  := Int     -- ^ number of reported infections
 ]

-- | SIR transition prior
transPriorSIR :: Observables env '["β",  "γ"] Double
  => Model env sig m (Double, Double)
transPriorSIR = do
  pBeta  <- gamma 2 1 #β
  pGamma <- gamma 1 (1/8) #γ
  return (pBeta, pGamma)

-- | Transition model from S and I
transSI :: Lookups popl '["s", "i", "r"] Int => TransModel env sig m Double (Record popl)
transSI  beta popl = do
  let (s_0, i_0, r_0 ) = (popl ^. s,  popl ^. i,  popl ^. r)
      pop = s_0 + i_0 + r_0
  dN_SI <- binomial' s_0 (1 - exp ((-beta * fromIntegral i_0) / fromIntegral pop))
  return $ popl & s .~ (s_0 - dN_SI)
                & i .~ (i_0 + dN_SI)

-- | Transition model from I and R
transIR :: Lookups popl '["i", "r"] Int => TransModel env sig m Double (Record popl)
transIR  gamma popl = do
  let (i_0, r_0) = (popl ^. i,  popl ^. r)
  dN_IR <- binomial' i_0 (1 - exp (-gamma))
  return $ popl & i .~ (i_0 - dN_IR)
                & r .~ (r_0 + dN_IR)

-- | Transition model from S to I, and I to R
transSIR :: (Lookups popl '["s", "i", "r"] Int, Member (Writer [Record popl]) sig)
  => TransModel env sig m (Double, Double) (Record popl)
transSIR (beta, gamma) popl = do
  popl <- (transSI beta >=> transIR gamma) popl
  Model $ tell [popl]  -- a user effect for writing each latent SIR state to a stream [Record popl]
  return popl

-- | SIR observation prior
obsPriorSIR :: Observables env '["ρ"] Double
  => Model env sig m Double
obsPriorSIR = beta 2 7 #ρ

-- | SIR observation model
obsSIR :: Lookup s "i" Int => Observable env "𝜉" Int
  => ObsModel env sig m Double (Record s) Reported
obsSIR rho popl  = do
  let i_0 = popl ^. i
  poisson (rho * fromIntegral i_0) #𝜉

-- | SIR as HMM
hmmSIR ::forall env popl sig m. (Lookups popl '["s", "i", "r"] Int, Observables env '["𝜉"] Int, Observables env '["β", "ρ", "γ"] Double)
  => Int -> Record popl -> Model env sig m ([Record popl], Record popl)
hmmSIR n = runWriterM . hmmGen transPriorSIR obsPriorSIR transSIR obsSIR n

-- | Simulate from the SIR model
simulateSIR :: Sampler ([(Int, Int, Int)], [Reported])
simulateSIR = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
  -- Specify model environment
      sim_env_in :: Env SIRenv
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
  -- Simulate an epidemic over 100 days
  ((sir_trace, _), sim_env_out) <- SIM.simulate sim_env_in $ hmmSIR 100 sir_0
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIR values over 100 days
      sirs = map (\sir -> (sir ^. s, sir ^. i, sir ^. r)) sir_trace
  return (sirs, 𝜉s)

-- | MH inference from SIR model: ([ρ], [β])
inferSIR :: Sampler ([Double], [Double])
inferSIR = do
  -- Simulate some observed infections
  𝜉s <- snd <$> simulateSIR
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0     = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
  -- Specify model environment
      mh_env_in :: Env SIRenv
      mh_env_in = #β := [] <:> #γ := [0.0085] <:> #ρ := [] <:> #𝜉 := 𝜉s <:> nil
  -- Run MH inference over 5000 iterations
  mhTrace <- MH.mhRaw 5000 (hmmSIR 100 sir_0) mh_env_in nil (#β <:> #ρ <:> nil)
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

-- | SIRS transition prior
transPriorSIRS :: Observables env '["β", "η", "γ"] Double
  => Model env sig m (Double, Double, Double)
transPriorSIRS = do
  (pBeta, pGamma)  <- transPriorSIR
  pEta <- gamma 1 (1/8) #η
  return (pBeta, pGamma, pEta)

-- | Transition model from S to R
transRS :: Lookups popl '["s", "r"] Int => TransModel env sig m Double (Record popl)
transRS eta popl = do
  let (r_0, s_0) = (popl ^. r,  popl ^. s)
  dN_RS <- binomial' r_0 (1 - exp (-eta))
  return $ popl & r .~ (r_0 - dN_RS)
                & s .~ (s_0 + dN_RS)

-- | Transition model from S to I, I to R, and R to S
transSIRS :: Lookups popl '["s", "i", "r"] Int => TransModel env sig m (Double, Double, Double) (Record popl)
transSIRS (beta, gamma, eta) = transSI beta >=> transIR gamma >=> transRS eta

-- | SIRS as HMM
hmmSIRS :: forall popl env sig m. (Lookups popl '["s", "i", "r"] Int,
            Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ρ"] Double)
  => Int -> Record popl -> Model env sig m ([Record popl], Record popl)
hmmSIRS n = runWriterM . hmmGen transPriorSIRS obsPriorSIR transSIRS obsSIR n

-- | Simulate from SIRS model: ([(s, i, r)], [𝜉])
simulateSIRS :: Sampler ([(Int, Int, Int)], [Reported])
simulateSIRS = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: emptyRecord
  -- Specify model environment
      sim_env_in :: Env SIRSenv
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
  -- Simulate an epidemic over 100 days
  ((sir_trace, _), sim_env_out) <- SIM.simulate sim_env_in $ hmmSIRS 100 sir_0
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIRS values over 100 days
      sirs = map (\sir -> (sir ^. s, sir ^. i, sir ^. r)) sir_trace
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

-- | SIRSV transition prior
transPriorSIRSV :: Observables env '["β", "γ", "ω", "η"] Double
  => Model env sig m (Double, Double, Double, Double)
transPriorSIRSV  = do
  (pBeta, pGamma, pEta) <- transPriorSIRS
  pOmega <- gamma 1 (1/16) #ω
  return (pBeta, pGamma, pEta, pOmega)

-- | Transition model from S to V
transSV :: Lookups popl '["s", "v"] Int => TransModel env sig m Double (Record popl)
transSV omega popl  = do
  let (s_0, v_0) = (popl ^. s,  popl ^. v)
  dN_SV <- binomial' s_0 (1 - exp (-omega))
  return $ popl & s .~ (s_0 - dN_SV)
                & v .~ (v_0 + dN_SV)

-- | Transition model from S to I, I to R, R to S, and S to V
transSIRSV :: Lookups popl '["s", "i", "r", "v"] Int => TransModel env sig m (Double, Double, Double, Double) (Record popl)
transSIRSV (beta, gamma, eta, omega) =
  transSI beta >=> transIR gamma >=> transRS eta  >=> transSV omega

-- | SIRSV as HMM
hmmSIRSV :: (Lookups popl '["s", "i", "r", "v"] Int,
             Observables env '["𝜉"] Int, Observables env '["β", "η", "γ", "ω", "ρ"] Double)
  => Int -> Record popl -> Model env sig m ([Record popl], Record popl)
hmmSIRSV n = runWriterM . hmmGen transPriorSIRSV obsPriorSIR transSIRSV obsSIR n

-- | Simulate from SIRSV model : ([(s, i, r, v)], [𝜉])
simulateSIRSV :: Sampler ([(Int, Int, Int, Int)], [Reported])
simulateSIRSV = do
  -- Specify model input of 762 susceptible and 1 infected
  let sir_0      = #s @= 762 <: #i @= 1 <: #r @= 0 <: #v @= 0 <: emptyRecord
  -- Specify model environment
      sim_env_in :: Env SIRSVenv
      sim_env_in = #β := [0.7] <:> #γ := [0.009] <:> #η := [0.05] <:> #ω := [0.02] <:> #ρ := [0.3] <:> #𝜉 := [] <:> nil
  -- Simulate an epidemic over 100 days
  ((sir_trace, _), sim_env_out) <- SIM.simulate sim_env_in $ hmmSIRSV 100 sir_0
  -- Get the observed infections over 100 days
  let 𝜉s :: [Reported] = get #𝜉 sim_env_out
  -- Get the true SIRSV values over 100 days
      sirvs = map (\sirv -> (sirv ^. s, sirv ^. i, sirv ^. r, sirv ^. v)) sir_trace
  return (sirvs, 𝜉s)
