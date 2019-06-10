{-# LANGUAGE Rank2Types #-}

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
  spec_Identity
  spec_Interchange
  spec_Composition
  spec_Homorphism

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


spec_Identity :: Spec
spec_Identity =
  describe "apply Map" $ do
    it "should conserve id for lists" $
      property (applyId::[Int] -> Bool)
    it "should conserve id for Maybe " $
      property (applyId::Maybe Int -> Bool)

spec_Interchange :: Spec
spec_Interchange =
  describe "lifted function" $ do
    it "should be applied to list" $
      property (let ps :: [Int -> String]
                    ps = pure show
                 in applyInter ps)
    it "should be applied to Maybe" $
      property (let ps :: Maybe (Int -> String)
                    ps = pure show
                 in applyInter ps)

spec_Composition :: Spec
spec_Composition =
  describe "apply composition" $ do
    it "should be lifted for lists" $
      property (let f :: [Double -> Double]
                    f = [sqrt]
                    g :: [Double -> String]
                    g = [show]
                 in applyComp g f)
    it "should be lifted for Maybe" $
      property (let f :: Maybe (Double -> Double)
                    f = Just sqrt
                    g :: Maybe (Double -> String)
                    g = Just show
                 in applyComp g f)

spec_Homorphism :: Spec
spec_Homorphism = describe "apply" $ do
  it "should be homomorphic for Lists" $
    property (let p :: x -> [x]
                  p = pure
              in applyHomo p (show:: Int -> String))
  it "should be homomorphic for Maybe" $
    property (let p :: x -> Maybe x
                  p = pure
              in applyHomo p (show:: Int -> String))