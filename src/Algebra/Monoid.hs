{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Algebra.Monoid(Monoid(..), MonoidLaws(..)) where

import Prelude hiding(Semigroup(..), Monoid(..))
import Algebra.Base(Monoid(..), SemiGroup(..), MonoidLaws(..))
import Algebra.SemiGroup

instance Monoid [a] where
  zero = []
  {-# INLINE zero #-}

instance (Num a) => Monoid (Sum a) where
  zero = fromInteger 0
  {-# INLINE zero #-}

instance (Num a) => Monoid (Product a) where
  zero = fromInteger 1
  {-# INLINE zero #-}

instance (SemiGroup a, Monoid a, Eq a) => MonoidLaws a