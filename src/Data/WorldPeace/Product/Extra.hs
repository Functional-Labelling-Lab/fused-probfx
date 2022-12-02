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
  , pfoldr
  , pfoldl
  , GetMaybe(..)
  , DefaultProduct(..)
  ) where

import           Data.WorldPeace.Extra (IsMember(..))
import           Data.WorldPeace.Product (Product (..))

pmap :: (forall a. f a -> g a) -> Product f as -> Product g as
pmap _ Nil        = Nil
pmap f (Cons a p) = Cons (f a) $ pmap f p

pfoldr :: (forall a. f a -> x -> x) -> x -> Product f as -> x
pfoldr _ x Nil        = x
pfoldr f x (Cons a p) = a `f` pfoldr f x p

pfoldl :: (forall a. x -> f a -> x) -> x -> Product f as -> x
pfoldl _ x Nil        = x
pfoldl f x (Cons a p) = pfoldl f (x `f` a) p

class GetMaybe a as where
  productGetMaybe :: Product f as -> Maybe (f a)

instance GetMaybe a (a : as) where
  productGetMaybe (Cons a _) = Just a

instance GetMaybe a '[] where
  productGetMaybe Nil = Nothing

instance {-# OVERLAPS #-} GetMaybe a as => GetMaybe a (a' : as) where
  productGetMaybe (Cons _ p) = productGetMaybe p

instance {-# INCOHERENT #-} GetMaybe a as where
  productGetMaybe _ = undefined

class DefaultProduct (as :: [u]) where
  productDefault :: (forall (a :: u). f a) -> Product f as

instance DefaultProduct '[] where
  productDefault _ = Nil

instance {-# OVERLAPS #-} DefaultProduct as => DefaultProduct (a : as) where
  productDefault d = Cons d $ productDefault d

instance {-# INCOHERENT #-} DefaultProduct as where
  productDefault d = undefined


-- product :: b -> (f a -> b) -> Product f  -> b
-- product