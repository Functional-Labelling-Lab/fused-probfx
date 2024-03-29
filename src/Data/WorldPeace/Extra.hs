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

module Data.WorldPeace.Extra
  ( IsMember(..)
  , openUnionLift
  , openUnionMatch
  ) where

import           Data.Functor.Identity (Identity (..))
import           Data.Kind             (Constraint)
import           Data.Maybe            (Maybe)
import qualified Data.WorldPeace       as WP

class IsMember a as where
  unionLift :: f a -> WP.Union f as
  unionMatch :: WP.Union f as -> Maybe (f a)
  productGet :: WP.Product f as -> f a
  productSet :: f a -> WP.Product f as -> WP.Product f as

openUnionLift :: IsMember a as => a -> WP.OpenUnion as
openUnionLift = unionLift . Identity

openUnionMatch :: IsMember a as => WP.OpenUnion as -> Maybe a
openUnionMatch = fmap runIdentity . unionMatch

instance IsMember a (a : as) where
  unionLift a = WP.This a
  unionMatch (WP.This a) = Just a
  unionMatch (WP.That _) = Nothing
  productGet (WP.Cons a _) = a
  productSet a (WP.Cons _ p) = WP.Cons a p

instance {-# OVERLAPS #-} IsMember a as => IsMember a (a' : as) where
  unionLift a = WP.That $ unionLift a
  unionMatch (WP.This _) = Nothing
  unionMatch (WP.That u) = unionMatch u
  productGet (WP.Cons _ p) = productGet p
  productSet newA (WP.Cons a p) = WP.Cons a $ productSet newA p
