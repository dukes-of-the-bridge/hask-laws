{-|
Module      : Algebras
Description : Re writing of Algebra for training
Copyright   : (c) Filippo Mariotti 2019
                  MD Ortega 2019,
                  Daniel Pires 2019
License     : GPL-3
Maintainer  :
Stability   : experimental
Portability : POSIX


-}
module Algebras where

{-|
 given a set S
 An operation ⊗ : S x S ⟶ S (... so a binary operation)
 If ∀ a,b ∈ S, (a ⊗ b) ∈ S (S is closed on ⊗)
 And ∀ a,b,c ∈ S, (a ⊗ b) ⊗ c = a ⊗ (b ⊗ c) = a ⊗ b ⊗ c

 The structure (S, ⊗) is a semi-group.
-}
class SemiGroup a where
  (|+|) :: a -> a -> a
  {-# MINIMAL (|+|) #-}

{-|
  given a set S
  An operation ⊗ : S x S ⟶ S (... so a binary operation)
  If ∀ a,b ∈ S, (a ⊗ b) ∈ S (S is closed on ⊗)
  And ∀ a,b,c ∈ S, (a ⊗ b) ⊗ c = a ⊗ (b ⊗ c) = a ⊗ b ⊗ c
  ∃! e ∈ S , ∀ a ∈ S,  a ⊗ e = e ⊗ a = a

  The structure (S, ⊗, e) is a monoid.

  A Monoid is a 'SemiGroup'
-}

class (SemiGroup a) => Monoid a where
  zero :: a
  {-# MINIMAL zero #-}