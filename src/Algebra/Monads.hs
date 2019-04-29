module Algebra.Monads where

import Algebra.Functors

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
class (Algebra.Functors.Functor m) => Monad m where
  bind :: m a -> (a -> m b) -> m b
  (>>=) :: m a -> (a -> m b) -> m b
  flatten :: m (m a) -> m a
  pure :: a -> m a
  bind ma k = Algebra.Monads.flatten . Algebra.Functors.fmap k $ ma
  flatten mma = bind mma id
  (>>=) = bind
  {-# INLINABLE bind #-}
  {-# INLINABLE flatten #-}
  {-# INLINE (>>=) #-}
  {-# MINIMAL pure , (bind | flatten) #-}
