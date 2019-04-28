module InstancesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebras
import Instances

spec :: Spec
spec = do
  spec_list_semigroup
  spec_list_monoid

spec_list_semigroup :: Spec
spec_list_semigroup =
  describe "SemiGroup for Lists" $ do
    it "close on list addition using length homomorphism" $
      property (isHomorphism (length :: [Int] -> Int))
    it "validate associative laws" $
      property (isAssociative :: [Int] -> [Int] -> [Int] -> Bool)

spec_list_monoid :: Spec
spec_list_monoid = describe "Monoid for Lists" $ do
  it "have neutral element" $
    property (hasZero :: [Int] -> Bool)

{-|
  Set of Semigroup laws
-}
isHomorphism :: (Algebras.SemiGroup a, Algebras.SemiGroup b, Eq b) => (a -> b) -> a -> a -> Bool
isHomorphism h x y = h x |+| h y == h (x |+| y)

isAssociative :: (Algebras.SemiGroup a, Eq a) => a -> a -> a -> Bool
isAssociative xs ys zs =
      (xs |+| ys) |+| zs == xs |+| (ys |+| zs) &&
        xs |+| (ys |+| zs) == xs |+| ys |+| zs

{-|
  Additional Monoid laws
-}
hasZero :: (Algebras.Monoid a, Eq a) => a -> Bool
hasZero a = zero |+| a == a |+| zero && zero |+| a == a
