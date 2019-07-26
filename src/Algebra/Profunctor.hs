{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.Profunctor(Profunctor(..), ProfunctorLaws(..)) where

import Algebra.Base(Profunctor(..), ProfunctorLaws(..))
import Algebra.Optic(Lens(..), Prism(..), Adapter(..))
import Algebra.Product(cross)
import Algebra.CoProduct(plus)

instance Profunctor (Lens a b) where
  dimap f g (Lens vw upd) = Lens (vw . f) (g . upd . cross id f)
  {-# INLINE dimap #-}

instance Profunctor (Prism a b) where
  dimap f g (Prism match build) = Prism (plus g id . match . f) (g . build)
  {-# INLINE dimap #-}

instance Profunctor (Adapter a b) where
  dimap f g (Adapter from to) = Adapter (from . f) (g . to)
  {-# INLINE dimap #-}

instance ProfunctorLaws where
  dimapId f = dimap id id f == f
  dimapComp fa' fa fb fb' pab = dimap (fa . fa') (fb' . fb) pab == (dimap fa' fb' . dimap fa fb $ pab)
