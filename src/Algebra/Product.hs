module Algebra.Product
  ( cross
  ) where

cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g (x, y) = (f x, g y)
