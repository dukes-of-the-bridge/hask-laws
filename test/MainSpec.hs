module MainSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "main test" $ do
    it "placeholder" $ do
      "foo bar" `shouldBe` "foo bar"
