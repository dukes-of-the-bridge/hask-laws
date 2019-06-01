{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.Functor(Functor(..), FunctorLaws(..)) where

import Prelude hiding(Functor(..))
import Algebra.Base(Functor(..), FunctorLaws(..))

instance Functor [] where
  fmap f = foldr (\a xs -> f(a) : xs) []
  {-# INLINABLE fmap #-}

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)
  {-# INLINABLE fmap #-}

instance FunctorLaws where
  mapId fa = fmap id fa == id fa
  mapCompose f g fa = fmap (g . f) fa == (fmap g . fmap f $ fa)
  {-# INLINABLE mapId #-}
  {-# INLINABLE mapCompose #-}