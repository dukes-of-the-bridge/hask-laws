module Instances where

import Algebras

instance Algebras.SemiGroup [a] where
  (|+|) = (++)
  {-# INLINE (|+|) #-}

instance Algebras.Monoid [a] where
  zero = []
  {-# INLINE zero #-}

instance Algebras.SemiGroup Int where
  (|+|) = (+)
  {-# INLINE (|+|) #-}
