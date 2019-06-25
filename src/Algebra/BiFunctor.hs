{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.BiFunctor where

import Algebra.Base(BiFunctor(..), BiFunctorLaws(..))

instance BiFunctor (,) where
  bimap g h (a, b) = (g a, h b)


instance BiFunctorLaws where
  bimapId fab              = bimap id id fab == fab
  bimapCompose g h k l fab = bimap (h . g) (l . k) fab == (bimap h l . bimap g k $ fab)