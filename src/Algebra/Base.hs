{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
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

class SemiGroupLaws where
  associativeSG :: (Eq a, SemiGroup a) => a -> a -> a -> Bool
  {-# MINIMAL associativeSG #-}
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

class MonoidLaws where
  hasZero :: (Eq a, Monoid a) => a -> Bool
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

class FunctorLaws where
  mapId ::  (Eq (f a), Functor f) => f a -> Bool
  mapCompose::(Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
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
  (>>=) = bind
  {-# INLINABLE bind #-}
  {-# INLINE (>>=) #-}
  {-# MINIMAL pure , flatten  #-}

class MonadLaws where
  leftId :: (Eq (m b), Monad m) => (a -> m b) -> a -> Bool
  rightId :: (Eq (m a), Monad m) => m a -> Bool
  associativeM :: (Eq (m c), Monad m) => (a -> m b) -> (b -> m c) -> a -> Bool
  {-# MINIMAL leftId, rightId, associativeM #-}

{-|
  A pro functor is the archetype of the consumer/producer pattern
  where a profunctor P A B will consume values of type A and produce
  values of type B.
  The @dimap@ operator is covariant on the type of the consumed value.

  As for functors both the laws
  - of conservation of id
  - composition of arrows

  must be verified
|-}
class ProFunctor p where
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (b -> d) -> p a b -> p a d
  rmap = dimap id
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}
  {-# MINIMAL dimap | (lmap, rmap)  #-}

class ProFunctorLaws where
  dimapId :: (Eq (p a b), ProFunctor p) => p a b -> Bool
  dimapComp :: (ProFunctor p) => (a'' ->  a') -> (a' -> a) -> (b -> b') -> (b' -> b'') -> p a b -> Bool