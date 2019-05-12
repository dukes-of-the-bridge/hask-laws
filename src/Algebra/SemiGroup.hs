module Algebra.SemiGroup(
  SemiGroup(..), Sum(..), Product(..)
) where

import Prelude hiding (Semigroup(..))
import Algebra.Base (SemiGroup(..))
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

