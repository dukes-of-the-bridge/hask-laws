module Algebra.Generator.Instances where

import Test.Hspec
import Test.QuickCheck
import Algebra.SemiGroup.Instances

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = fmap Sum arbitrary

instance Arbitrary a => Arbitrary (Product a) where
  arbitrary = fmap Product arbitrary
