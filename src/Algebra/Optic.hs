module Algebra.Optic
  ( Lens(..)
  , Prism(..)
  , Adapter(..)
  ) where

import Algebra.Base (Profunctor (..))

data Lens a b s t = Lens
  { view   :: s -> a
  , update :: (b, s) -> t
  }

data Prism a b s t = Prism
  { match :: s -> Either t a
  , build :: b -> t
  }

data Adapter a b s t = Adapter
  { from :: s -> a
  , to   :: b -> t
  }
