module Algebra.ApplicativeSpec(spec) where

import Prelude hiding(Applicative(..), Functor(..), pure, (<*>), (<$>), fmap)
import Algebra.Applicative(Applicative(..), ApplicativeLaws(..))
import Algebra.Base(Applicative(..), ApplicativeLaws(..))
import Algebra.Functor
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  spec_applyMap
  spec_applyId
  spec_applyLifted

spec_applyMap :: Spec
spec_applyMap =
  describe "apply Map" $ do
    it "should verify conservation of fmap for lists" $
      property (let s :: Int -> String
                    s = show
                in applyMap s :: [Int] -> Bool)
    it "should verify conservation of  fmap for Maybe" $
      property ( let s :: Int -> String
                     s = show
                 in applyMap s :: Maybe Int -> Bool)


spec_applyId :: Spec
spec_applyId =
  describe "apply Map" $ do
    it "should conserve id for lists" $
      property (applyId::[Int] -> Bool)
    it "should conserve id for Maybe " $
      property (applyId::Maybe Int -> Bool)

spec_applyLifted :: Spec
spec_applyLifted =
  describe "lifted function" $ do
    it "should be applied to list" $
      property (let ps :: [Int -> String]
                    ps = pure show
                 in applyLifted ps)
    it "should be applied to Maybe" $
      property (let ps :: Maybe (Int -> String)
                    ps = pure show
                 in applyLifted ps)