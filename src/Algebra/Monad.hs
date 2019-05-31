module Algebra.Monad(Monad(..)) where

import Prelude hiding(Monad(..), Functor(..))
import Algebra.Base(Functor(..), Monad(..))
import Algebra.Functor

instance Monad [] where
  pure a    = [a]
  flatten   = foldr (++) []
