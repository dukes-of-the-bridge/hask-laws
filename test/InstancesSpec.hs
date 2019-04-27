module InstancesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebras
import Instances

spec :: Spec
spec = describe "SemiGroup for Lists" $ do
  it "close on list addition using length homomorphism" $
    property $
      \xs ys -> length (xs::[Int]) |+| length (ys::[Int]) == length (xs |+| ys)
  it "validate associative laws with grouping" $
    property $
      \xs ys zs-> ((xs::Int) |+| (ys::Int)) |+| (zs::Int) == (xs::Int) |+| ((ys::Int) |+| (zs::Int))
  it "validate associative laws without grouping" $
    property $
      \xs ys zs-> ((xs::Int) |+| (ys::Int)) |+| (zs::Int) == (xs::Int) |+| (ys::Int) |+| (zs::Int)
