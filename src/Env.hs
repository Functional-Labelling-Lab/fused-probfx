{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
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
{-# LANGUAGE FunctionalDependencies #-}
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

import           Data.Kind             (Constraint)
import           Data.Proxy            (Proxy (Proxy))
import qualified Data.WorldPeace       as WP
import qualified Data.WorldPeace.Extra as WPE
import           GHC.OverloadedLabels  (IsLabel (..))
import           GHC.TypeLits          (KnownSymbol, Symbol, symbolVal)

-- | Containers for observable variables
data ObsVar (x :: Symbol) where
  ObsVar :: KnownSymbol x => ObsVar x

-- | Allows the syntax @#x@ to be automatically lifted to the type @ObsVar "x"@.
instance (KnownSymbol x, x ~ x') => IsLabel x (ObsVar x') where
  fromLabel = ObsVar

-- | Convert an observable variable from a type-level string to a value-level string
varToStr :: forall x. ObsVar x -> String
varToStr ObsVar = symbolVal (Proxy @x)

-- | Class for implementing the construction of a product type
class ConstructProduct 
    (f :: u -> *) -- ^ Product interpretation type
    (a :: u)      -- ^ New type for the product
    (as :: [u])   -- ^ Already existing product arguments (for conditioning the product construction on)
    (c :: *)      -- ^ Input type to the join operator
    | f c -> a, f a -> c where
  -- | Implements conversion from a join operator argument to the product representation
  constructProduct :: c -> f a

-- | Join operator for the product types that implement 'ConstructProduct'
infixr 5 <:>
(<:>) :: forall f a as c. ConstructProduct f a as c => c -> WP.Product f as -> WP.Product f (a : as)
x <:> p = WP.Cons (constructProduct @_ @f @a @as @c x) p

-- | Construct empty product type
nil :: WP.Product (f :: u -> *) '[]
nil = WP.Nil

instance HasObsVar x env ~ False => ConstructProduct EnvElem (x := a) env (Assign (ObsVar x) [a]) where
  constructProduct (_ := as) = Elem as

-- | Assign or associate a variable @x@ with a value of type @a@
data Assign x a = x := a

-- | Product interpretation for holding environment entries
data EnvElem (e :: Assign Symbol *) where
  Elem :: [a] -> EnvElem (x := a)

-- | Type for the probabilistic environment
type Env = WP.Product EnvElem

-- | Constraint for a probabilistic variable having a certan type 
--   in the environment
type Observable env x a = WPE.IsMember (x := a) env

-- | Get a value list out of the environment
get :: forall a x env. WPE.IsMember (x := a) env => ObsVar x -> Env env -> [a]
get _ env = let (Elem as) = WPE.productGet @(x := a) env in as

-- | Set the value list for a specific variable in the environment
set :: forall a x env. WPE.IsMember (x := a) env => ObsVar x -> [a] -> Env env -> Env env
set _ as env = WPE.productSet @(x := a) (Elem as) env

-- | Creates 'Observable' constraints for multiple variables
type family Observables env (ks :: [Symbol]) a :: Constraint where
  Observables env (x ': xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

-- | Check if a variable is already in the environment
type family HasObsVar x env where
  HasObsVar x ((x := _) : _) = True
  HasObsVar x (_ : xs) = HasObsVar x xs
  HasObsVar _ '[] = False

-- | Extract a list of variables from the environment
type family ExtractVars (e :: [Assign Symbol *]) :: [Symbol] where
  ExtractVars '[] = '[]
  ExtractVars ((x := _) : e) = x : ExtractVars e

-- | Extract a list of all types used for variables in the environment
type family ExtractTypes (e :: [Assign Symbol *]) :: [*] where
  ExtractTypes '[] = '[]
  ExtractTypes ((_ := a) : e) = a : WP.Remove a (ExtractTypes e)
