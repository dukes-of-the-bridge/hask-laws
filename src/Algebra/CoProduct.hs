module Algebra.CoProduct
  ( plus
  ) where

plus :: (a -> c) -> (b -> d) -> Either a b -> Either c d
plus f g (Left x)  = Left (f x)
plus _ g (Right y) = Right (g y)

