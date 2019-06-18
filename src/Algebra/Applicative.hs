{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Algebra.Applicative(Applicative(..), ApplicativeLaws(..)) where

import Prelude hiding(Applicative(..), Functor(..), pure, (<*>), (<$>), fmap)
import Algebra.Base(Functor(..), Applicative(..), ApplicativeLaws(..))
import Algebra.Functor
import Data.Typeable (Proxy)

instance Applicative [] where
  pure a = [a]
  apply fs xs = foldl (\bs f -> bs ++ fmap f xs) [] fs
  {-# INLINABLE pure #-}
  {-# INLINABLE apply #-}

instance Applicative Maybe where
  pure = Just
  apply (Just f) (Just a) = Just (f a)
  apply _ _               = Nothing
  {-# INLINE pure #-}
  {-# INLINABLE apply #-}

{-|
  The laws are

(pure f) |*| x = fmap f x
pure id |*| v = v
pure (.) |*| u |*| v |*| w = u |*| (v |*| w)
pure f |*| pure x = pure (f x)
u |*| pure y = pure ($ y) |*| u
|-}
instance ApplicativeLaws where
  applyMap f fa      = pure f  |*| fa == fmap f fa
  applyId fa         = pure id |*| fa == fa
  applyInter pf y    = pf |*| pure y  == pure ($ y) |*| pf
  applyComp pg pf fa = pure (.) |*| pg |*| pf |*| fa == pg |*| (pf |*| fa)
  applyHomo          = homomorphism

homomorphism :: forall a b f.(Eq (f b), Applicative f) => Proxy f -> (a -> b) -> a -> Bool
homomorphism _ g x = (pure (g x) :: f b) == (pure g |*| (pure x :: f a))

