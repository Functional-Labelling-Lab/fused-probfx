{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.WorldPeace.Product.Extra
  ( pmap
  , Elem
  , PElem
  ) where

import           Data.WorldPeace.Product (Product (..))

-- | Type-level version of the 'elem' function.
--
-- >>> Refl :: Elem String '[Double, String, Char] :~: 'True
-- Refl
-- >>> Refl :: Elem String '[Double, Char] :~: 'False
-- Refl
type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem _ '[]       = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs

class PElem (a :: k) (as :: [k]) where
  get :: Product f as -> f a
  set :: f a -> Product f as -> Product f as

instance PElem a (a ': as) where
  get (Cons a _) = a
  set a (Cons _ p) = Cons a p

instance PElem a as => PElem a (a' : as) where
  get (Cons _ p) = get p
  set a (Cons a' p) = Cons a' $ set a p

pmap :: (forall a. f a -> g a) -> Product f as -> Product g as
pmap _ Nil        = Nil
pmap f (Cons a p) = Cons (f a) $ pmap f p

pfoldr :: (forall a. f a -> x -> x) -> x -> Product f as -> x
pfoldr _ x Nil        = x
pfoldr f x (Cons a p) = a `f` pfoldr f x p

pfoldl :: (forall a. x -> f a -> x) -> x -> Product f as -> x
pfoldl _ x Nil        = x
pfoldl f x (Cons a p) = pfoldl f (x `f` a) p
