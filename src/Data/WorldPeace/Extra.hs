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
  ) where

import qualified Data.WorldPeace as WP
import Data.Maybe (Maybe)

class IsMember a as where
  unionLift :: f a -> WP.Union f as 
  unionMatch :: WP.Union f as -> Maybe (f a)
  productMatch :: WP.Product f as -> f a

instance IsMember a (a : as) where
  unionLift a = WP.This a
  unionMatch (WP.This a) = Just a
  unionMatch (WP.That _) = Nothing
  productMatch (WP.Cons a _) = a

instance IsMember a as => IsMember a (a' : as) where
  unionLift a = WP.That $ unionLift a
  unionMatch (WP.This _) = Nothing
  unionMatch (WP.That u) = unionMatch u
  productMatch (WP.Cons _ p) = productMatch p