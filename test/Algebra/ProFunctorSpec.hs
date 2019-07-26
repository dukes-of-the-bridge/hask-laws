module Algebra.ProFunctorSpec(spec) where


import Test.Hspec
import Test.QuickCheck
import Algebra.Base(Profunctor(..), ProfunctorLaws(..))
import Algebra.Profunctor(ProFunctor(..), ProFunctorLaws(..))


spec :: Spec
spec = describe "profunctor law" $ do
  it "should conserve id" $
    property (dimapId :: (Int -> String) -> Bool)