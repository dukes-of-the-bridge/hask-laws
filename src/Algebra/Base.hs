{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}

module Algebra.Base where

import Prelude hiding (Semigroup, Monoid, Functor, Applicative, Monad, fmap, flatten, pure)
import Data.Typeable (Proxy)
import Data.Proxy (KProxy)
import Data.Kind (Type)

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
, Applicative f
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
  {-# MINIMAL mapId, mapCompose #-}

{-|
  An applicative algebra describes the application to an effect
  of a transformation issued from an effect of the same type

  apply :: f (a -> b) -> f a -> f b

  The laws are

(pure f) |*| x = fmap f x
pure id |*| v = v
pure (.) |*| u |*| v |*| w = u <*> (v <*> w)
pure f |*| pure x = pure (f x)
u |*| pure y = pure ($ y) |*| u
|-}
class (Functor f) => Applicative f where
  pure :: a -> f a
  apply :: f (a -> b) -> f a -> f b
  (|*|) :: f (a -> b) -> f a -> f b
  (|$|) :: (a -> b) -> f a -> f b
  (|*|) = apply
  (|$|) = fmap
  {-# INLINE (|*|) #-}
  {-# INLINE (|$|) #-}
  {-# MINIMAL pure, apply #-}


class ApplicativeLaws  where
  applyMap :: (Eq (f b), (Applicative f)) => (a -> b) -> f a -> Bool
  applyId :: (Eq (f a), (Applicative f)) => f a -> Bool
  applyHomo :: forall a b f.(Eq (f b), Applicative f) => Proxy f -> (a -> b) -> a -> Bool
  applyInter :: (Eq (f b), (Applicative f)) => f (a -> b) -> a -> Bool
  applyComp :: (Eq (f c), (Applicative f)) => f (b -> c) -> f (a -> b) -> f a -> Bool
  {-# MINIMAL applyMap, applyId, applyInter, applyComp, applyHomo #-}

{-|
  A Monad is an Algebra built on a Functor algebra equiped with a flatten function.
  The complete description of the effect propagation is being provided by a
  bind function

  The laws are

> bind ma pure ==  ma
> bind (pure a) k == k a
> pending :)
-}
class (Functor m, Applicative m) => Monad m where
  bind :: m a -> (a -> m b) -> m b
  (>>=) :: m a -> (a -> m b) -> m b
  flatten :: m (m a) -> m a
  bind ma k = flatten . fmap k $ ma
  (>>=) = bind
  {-# INLINABLE bind #-}
  {-# INLINE (>>=) #-}
  {-# MINIMAL flatten  #-}

class MonadLaws where
  leftId :: (Eq (m b), Monad m) => (a -> m b) -> a -> Bool
  rightId :: (Eq (m a), Monad m) => m a -> Bool
  associativeM :: (Eq (m c), Monad m) => (a -> m b) -> (b -> m c) -> a -> Bool
  {-# MINIMAL leftId, rightId, associativeM #-}