{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.Monad(Monad(..), MonadLaws(..)) where

import Prelude hiding(Monad(..), Functor(..), Applicative(..), pure)
import Algebra.Base(Functor(..), Applicative(..), Monad(..), MonadLaws(..))
import Algebra.Functor
import Algebra.Applicative

instance Monad [] where
  flatten   = foldr (++) []
  {-# INLINABLE flatten #-}

instance Monad Maybe where
  flatten (Just (Just a)) = Just a
  flatten _               = Nothing
  {-# INLINABLE flatten #-}

instance MonadLaws where
  leftId f a = (pure a >>= f) == f a
  rightId ma = (ma >>= pure) == ma
  associativeM f g a = (pure a >>= f >>= g) == (pure a >>= (\a -> f a >>= g))
  {-# INLINABLE leftId #-}
  {-# INLINABLE rightId #-}
  {-# INLINABLE associativeM #-}
