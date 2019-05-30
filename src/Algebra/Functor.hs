{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Algebra.Functor() where

import Prelude hiding(Functor(..))
import Algebra.Base(Functor(..), FunctorLaw(..))

instance Functor [] where
  fmap f = foldr (\a xs -> f(a) : xs) []
  {-# INLINABLE fmap #-}

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

instance (Functor f) => FunctorLaw f where
  mapId fa = fmap id fa == id fa