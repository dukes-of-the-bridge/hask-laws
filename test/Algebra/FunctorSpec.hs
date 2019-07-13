module Algebra.FunctorSpec(spec) where

import Prelude hiding (Functor(..))
import Test.Hspec
import Test.QuickCheck
import Algebra.Base(Functor(..), FunctorLaws(..))
import Algebra.Functor


spec::Spec
spec = do
  spec_map_id
  spec_composition

spec_map_id :: Spec
spec_map_id  = describe "Functor Laws" $ do
   it "should preserve identity function for Lists" $
     property (preserveId :: [Int] -> Bool)
   it "should preserve function for Maybe" $
     property (preserveId :: Maybe Int -> Bool)

spec_composition::Spec
spec_composition = describe "Functor Laws" $ do
  it "should preserve composition for lists" $
    property (preserveCompose (7 *) show :: [Int] -> Bool)
  it "should preserve composition for lists" $
    property (preserveCompose (7 *) show :: Maybe Int -> Bool)
