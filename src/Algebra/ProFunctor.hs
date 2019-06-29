{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.ProFunctor(ProFunctor(..), ProFunctorLaws(..)) where

import Algebra.Base(ProFunctor(..), ProFunctorLaws(..))

instance ProFunctor (->) where
  dimap f h g = h . g . f

instance ProFunctorLaws where
  dimapId f = dimap id id f == f
