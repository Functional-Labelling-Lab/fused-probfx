{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

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
  , HasObsVar
  , Observable
  , Observables
  , (<:>)
  , nil
  ) where

import           Data.Kind                     (Constraint)
import           Data.Proxy                    (Proxy (Proxy))
import           Data.WorldPeace.Product       (Product (Cons, Nil))
import           Data.WorldPeace.Product.Extra (Elem)
import           Data.WorldPeace.Union         (Union)
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

-- | Assign or associate a variable @x@ with a value of type @a@
data Assign x a = x := a

data EnvElem (e :: Assign Symbol *) where
  Elem :: [a] -> EnvElem (x := a)

type Env = Product EnvElem

-- | Empty model environment
nil :: Env '[]
nil = Nil

infixr 5 <:>
-- | Prepend a variable assignment to a model environment
(<:>) :: HasObsVar x env ~ False => Assign (ObsVar x) [a] -> Env env -> Env ((x := a) : env)
(_ := as) <:> env = Cons (Elem as) env

type Observable env x a = (Elem (x := a) env ~ True)

type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

type family HasObsVar (x :: Symbol) (env :: [Assign Symbol *]) where
  HasObsVar x (x := _ : _) = True
  HasObsVar x (x' := _ : env) = HasObsVar x env
  HasObsVar _ '[] = False

