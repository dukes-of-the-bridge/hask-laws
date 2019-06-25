module Algebra.BiFunctorSpec(spec) where

import Test.Hspec
import Test.QuickCheck
import Algebra.Base(BiFunctor(..), BiFunctorLaws(..))
import Algebra.BiFunctor(BiFunctor(..), BiFunctorLaws(..))

spec :: Spec
spec = do
  spec_id
  spec_map

spec_id :: Spec
spec_id = describe "Bi Functor laws" $ do
  it "should preserve id in" $
    property (bimapId :: (Int, String) -> Bool)
  it "preserve id in" $
    property (bimapId :: (Int, String) -> Bool)


spec_map :: Spec
spec_map = describe "Bi Functor laws" $ do
  it "should preserve composition" $
    property (bimapCompose show dup dup dup :: (Int, String) -> Bool)
  it "should preserve composition" $
    property (bimapCompose show dup dup dup :: Either Int String -> Bool)

dup :: String -> String
dup s = s ++ s