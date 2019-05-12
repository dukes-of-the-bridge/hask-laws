module Algebra.MonoidSpec(spec) where

import Prelude hiding (Monoid(..), Semigroup(..))
import Test.Hspec
import Test.QuickCheck
import Algebra.SemiGroup
import Algebra.Monoid
import Algebra.Generator.Instances


spec :: Spec
spec = do
  spec_semigroup
  spec_monoid


spec_semigroup :: Spec
spec_semigroup =
  describe "Semigroup type class" $ do
    it "validate associative laws for lists" $
      property
          (let checkLists :: (Arbitrary a, Show a) => ([a] -> [a] -> [a] -> Bool) -> [a] -> [a] -> [a] -> Property
               checkLists prop xs ys zs = classifyLists [xs, ys, zs] prop
           in checkLists (isAssociative :: [Int] -> [Int] -> [Int] -> Bool))
    it "validate associative laws for Sum Int" $
      property (isAssociative :: Sum Int -> Sum Int -> Sum Int -> Bool)
    it "validate associative laws for Product Int" $
      property (isAssociative :: Product Int -> Product Int -> Product Int -> Bool)


spec_monoid :: Spec
spec_monoid = describe "Monoid type class" $ do
  it "has neutral element for list" $
    property (hasZero :: [Int] -> Bool)
  it "has neutral element for Sum Int" $
    property (hasZero :: Sum Int -> Bool)
  it "has neutral element for Product Int" $
    property (hasZero :: Product Int -> Bool)


classifyLists :: (Testable prop) => [[a]] -> prop -> Property
classifyLists as = classify (foldr (\x b -> null x || b) False as) "empty list"


--isHomomorphic :: (SemiGroup a, SemiGroup b, Eq b) => (a -> b) -> a -> a -> Bool
--isHomomorphic h x y = h x |+| h y == h (x |+| y)


