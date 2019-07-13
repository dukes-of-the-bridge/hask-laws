{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.BiFunctor(BiFunctor(..), BiFunctorLaws(..)) where

import Algebra.Base(BiFunctor(..), BiFunctorLaws(..))

instance BiFunctor (,) where
  bimap g h (a, b) = (g a, h b)
  {-# INLINE bimap #-}


instance BiFunctor Either where
  bimap g _ (Left a)  = Left  (g a)
  bimap _ h (Right b) = Right (h b)
  {-# INLINABLE bimap #-}


instance BiFunctorLaws where
  preserveIds      fab         = bimap id id fab == fab
  preserveComposes g h k l fab = bimap (h . g) (l . k) fab == (bimap h l . bimap g k $ fab)
