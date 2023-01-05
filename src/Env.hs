{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

{- | This implements the model environments that users must provide upon running a model;
     such environments assign traces of values to the "observable variables" (random
     variables which can be conditioned against) of a model.
-}

module Env
  ( -- * Observable variable
    ObsVar(..)
  , varToStr
    -- * Model environment
  , Assign(..)
  , Env(..)
  , EnvElem(..)
  , Observable(..)
  , Observables(..)
  , ConstructProduct(..)
  , nil
  , get
  , set
  , HasObsVar
  , ExtractVars
  , ExtractTypes
  , (<:>)
  ) where

import           Data.Kind                     (Constraint)
import           Data.Proxy                    (Proxy (Proxy))
import qualified Data.WorldPeace               as WP
import qualified Data.WorldPeace.Extra         as WPE
import           FindElem                      (FindElem (..), Idx (..))
import           GHC.OverloadedLabels          (IsLabel (..))
import           GHC.TypeLits                  (KnownSymbol, Symbol, symbolVal)

-- | Containers for observable variables
data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

-- | Allows the syntax @#x@ to be automatically lifted to the type @ObsVar "x"@.
instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

-- | Convert an observable variable from a type-level string to a value-level string
varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

class ConstructProduct (f :: u -> *) (a :: u) (c :: *) | f c -> a, f a -> c where
  constructProduct :: c -> f a

infixr 5 <:>
(<:>) :: ConstructProduct f a c => c -> WP.Product f as -> WP.Product f (a : as)
x <:> p = WP.Cons (constructProduct x) p

nil :: WP.Product (f :: u -> *) '[]
nil = WP.Nil

instance ConstructProduct EnvElem (x := a) (Assign (ObsVar x) [a]) where
  constructProduct (x := as) = Elem as

-- | Assign or associate a variable @x@ with a value of type @a@
data Assign x a = x := a

data EnvElem (e :: Assign Symbol *) where
  Elem :: [a] -> EnvElem (x := a)

type Env = WP.Product EnvElem

type Observable env x a = WPE.IsMember (x := a) env

get :: forall a x env. WPE.IsMember (x := a) env => ObsVar x -> Env env -> [a]
get _ env = let (Elem as) = WPE.productGet @(x := a) env in as

set :: forall a x env. WPE.IsMember (x := a) env => ObsVar x -> [a] -> Env env -> Env env
set _ as env = WPE.productSet @(x := a) (Elem as) env

type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

type family HasObsVar x env where
  HasObsVar x ((x := _) : _) = True
  HasObsVar x (_ : xs) = HasObsVar x xs
  HasObsVar _ '[] = False

type family ExtractVars (e :: [Assign Symbol *]) :: [Symbol] where
  ExtractVars '[] = '[]
  ExtractVars ((x := _) : e) = x : WP.Remove x (ExtractVars e)

type family ExtractTypes (e :: [Assign Symbol *]) :: [*] where
  ExtractTypes '[] = '[]
  ExtractTypes ((_ := a) : e) = a : WP.Remove a (ExtractTypes e)