module Algebra.Monoids where

import Algebra.SemiGroups

{-|
  given a set S

> An operation ⊗ : S x S ⟶ S (... so a binary operation)
> If ∀ a,b ∈ S, (a ⊗ b) ∈ S (S is closed on ⊗)
> And ∀ a,b,c ∈ S, (a ⊗ b) ⊗ c = a ⊗ (b ⊗ c) = a ⊗ b ⊗ c
> ∃! e ∈ S , ∀ a ∈ S,  a ⊗ e = e ⊗ a = a

The structure (S, ⊗, e) is a monoid.

  A Monoid is a 'SemiGroup'
-}
class (SemiGroup a) => Monoid a where
  zero :: a
  {-# MINIMAL zero #-}


instance Algebra.Monoids.Monoid [a] where
  zero = []
  {-# INLINE zero #-}