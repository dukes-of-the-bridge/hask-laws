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
  describe "Semigroup Laws" $ do
    it "should verify associativity for List" $
      property
          (let checkLists :: (Arbitrary a, Show a) => ([a] -> [a] -> [a] -> Bool) -> [a] -> [a] -> [a] -> Property
               checkLists prop xs ys zs = classifyLists [xs, ys, zs] prop
           in checkLists (associativeSG :: [Int] -> [Int] -> [Int] -> Bool))
    it "should verify associativity for Sum Int" $
      property (associativeSG :: Sum Int -> Sum Int -> Sum Int -> Bool)
    it "should verify associativity for Product Int" $
      property (associativeSG :: Product Int -> Product Int -> Product Int -> Bool)

spec_monoid :: Spec
spec_monoid = describe "Monoid Laws" $ do
  it "has neutral element for list" $
    property (hasZero :: [Int] -> Bool)
  it "has neutral element for Sum Int" $
    property (hasZero :: Sum Int -> Bool)
  it "has neutral element for Product Int" $
    property (hasZero :: Product Int -> Bool)

classifyLists :: (Testable prop) => [[a]] -> prop -> Property
classifyLists as = classify (foldr (\x b -> null x || b) False as) "empty list"
