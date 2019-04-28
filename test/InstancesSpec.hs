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
    it "close on list addition" $
      property
        (let checkLists :: (Arbitrary a, Show a) => ([a] -> [a] -> Bool) -> [a] -> [a] -> Property
             checkLists prop xs ys = classifyLists [xs, ys] prop
          in checkLists (isHomomorphic (length :: [Int] -> Int)))
    it "validate associative laws" $
      property
          (let checkLists :: (Arbitrary a, Show a) => ([a] -> [a] -> [a] -> Bool) -> [a] -> [a] -> [a] -> Property
               checkLists prop xs ys zs = classifyLists [xs, ys, zs] prop
           in checkLists (isAssociative :: [Int] -> [Int] -> [Int] -> Bool))

spec_list_monoid :: Spec
spec_list_monoid = describe "Monoid for Lists" $
  it "have neutral element" $
    property (hasZero :: [Int] -> Bool)

classifyLists :: (Testable prop) => [[a]] -> prop -> Property
classifyLists as = classify (foldr (\x b -> null x || b) False as) "empty list"

{-|
  Set of Semigroup laws
-}
isHomomorphic :: (Algebras.SemiGroup a, Algebras.SemiGroup b, Eq b) => (a -> b) -> a -> a -> Bool
isHomomorphic h x y = h x |+| h y == h (x |+| y)

isAssociative :: (Algebras.SemiGroup a, Eq a) => a -> a -> a -> Bool
isAssociative xs ys zs =
      (xs |+| ys) |+| zs == xs |+| (ys |+| zs) &&
        xs |+| (ys |+| zs) == xs |+| ys |+| zs

{-|
  Additional Monoid laws
-}
hasZero :: (Algebras.Monoid a, Eq a) => a -> Bool
hasZero a = zero |+| a == a |+| zero && zero |+| a == a
