{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.Monad(Monad(..), MonadLaws(..)) where

import Prelude hiding(Monad(..), Functor(..), pure)
import Algebra.Base(Functor(..), Monad(..), MonadLaws(..))
import Algebra.Functor

instance Monad [] where
  pure a    = [a]
  flatten   = foldr (++) []
  {-# INLINABLE pure #-}
  {-# INLINABLE flatten #-}

instance Monad Maybe where
  pure = Just
  flatten (Just (Just a)) = Just a
  flatten _               = Nothing
  {-# INLINE pure #-}
  {-# INLINABLE flatten #-}

instance MonadLaws where
  leftId f a = (pure a >>= f) == f a
  rightId ma = (ma >>= pure) == ma
  associativeM f g a = (pure a >>= f >>= g) == (pure a >>= (\a -> f a >>= g))
  {-# INLINABLE leftId #-}
  {-# INLINABLE rightId #-}
  {-# INLINABLE associativeM #-}
