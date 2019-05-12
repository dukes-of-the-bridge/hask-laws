{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Algebra.SemiGroup(
  SemiGroup(..), SemiGroupLaws(..), Sum(..), Product(..)
) where

import Prelude hiding (Semigroup(..))
import Algebra.Base (SemiGroup(..), SemiGroupLaws(..))

import Algebra.SemiGroup.Instances

instance (Num a) => SemiGroup (Sum a) where
  (|+|) = (+)
  {-# INLINE (|+|) #-}

instance (Num a) => SemiGroup (Product a) where
  (|+|) = (*)
  {-# INLINE (|+|) #-}

instance SemiGroup [a] where
  (|+|) = (++)
  {-# INLINE (|+|) #-}

instance (SemiGroup a, Eq a) => SemiGroupLaws a where
  isAssociative ::  (Eq a) => a -> a -> a -> Bool
  isAssociative xs ys zs =
        (xs |+| ys) |+| zs == xs |+| (ys |+| zs) &&
          xs |+| (ys |+| zs) == xs |+| ys |+| zs
