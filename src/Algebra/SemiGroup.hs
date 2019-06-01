{-# LANGUAGE MultiParamTypeClasses #-}

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

instance SemiGroupLaws  where
  associativeSG xs ys zs =
        (xs |+| ys) |+| zs == xs |+| (ys |+| zs) &&
          xs |+| (ys |+| zs) == xs |+| ys |+| zs
  {-# INLINABLE associativeSG #-}
