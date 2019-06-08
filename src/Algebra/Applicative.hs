module Algebra.Applicative(Applicative(..), ApplicativeLaws(..)) where

import Prelude hiding(Applicative(..), Functor(..), pure, (<*>), (<$>), fmap)
import Algebra.Base(Functor(..), Applicative(..), ApplicativeLaws(..))
import Algebra.Functor

instance Applicative [] where
  pure a = [a]
  apply fs xs = foldl (\bs f -> bs ++ fmap f xs) [] fs
  {-# INLINABLE pure #-}
  {-# INLINABLE apply #-}

instance Applicative Maybe where
  pure = Just
  apply (Just f) (Just a) = Just (f a)
  apply _ _               = Nothing
  {-# INLINE pure #-}
  {-# INLINABLE apply #-}
