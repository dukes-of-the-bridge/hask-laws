module Algebra.Functors where

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
  lift = Algebra.Functors.fmap

instance Algebra.Functors.Functor [] where
  fmap f = foldr (\a xs -> f(a) : xs) []
