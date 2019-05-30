{-# LANGUAGE ConstrainedClassMethods #-}

module Algebra.Base where

import Prelude hiding (Semigroup, Monoid, Functor, Monad, fmap, flatten)

{-|
 given a set S

> An operation ⊗ : S x S ⟶ S (... so a binary operation)
> If ∀ a,b ∈ S, (a ⊗ b) ∈ S (S is closed on ⊗)
> And ∀ a,b,c ∈ S, (a ⊗ b) ⊗ c = a ⊗ (b ⊗ c) = a ⊗ b ⊗ c

 The structure (S, ⊗) is a semi-group.
-}
class SemiGroup a where
  (|+|) :: a -> a -> a
  {-# MINIMAL (|+|) #-}

class (SemiGroup a) => SemiGroupLaws a where
  isAssociative :: a -> a -> a -> Bool
  {-# MINIMAL isAssociative #-}
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

class (Monoid a) => MonoidLaws a where
  hasZero :: a -> Bool
  {-# MINIMAL hasZero #-}

{-|
  A functor algebra describes the preservation of the structure of an effect
  when a function is being lifted from a source Category to a target Category
  in the case of Haskell we only define EndoFunctor Algebras from Hask to Hask
  Functor laws are

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g
-}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  lift :: (a -> b) -> f a -> f b
  {-# MINIMAL fmap #-}
  lift = fmap

class (Functor f) => FunctorLaw f where
  mapId ::  (Eq (f a)) => f a -> Bool
  mapCompose::(Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
  {-# minimal mapId, mapCompose #-}
  
{-|
  A Monad algebra describes the propagation of an effect thru the application of
  a Kleisli construct a -> m b
  Monad laws are

  A Monad is an Algebra built on a Functor algebra equiped with a flatten function.
  The complete description of the effect propagation is being provided by a
  bind function

  The laws are

> bind ma pure ==  ma
> bind (pure a) k == k a
> pending :)
-}
class (Functor m) => Monad m where
  bind :: m a -> (a -> m b) -> m b
  (>>=) :: m a -> (a -> m b) -> m b
  flatten :: m (m a) -> m a
  pure :: a -> m a
  bind ma k = flatten . fmap k $ ma
  flatten mma = bind mma id
  (>>=) = bind
  {-# INLINABLE bind #-}
  {-# INLINABLE flatten #-}
  {-# INLINE (>>=) #-}
  {-# MINIMAL pure , (bind | flatten) #-}
