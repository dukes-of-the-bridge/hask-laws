module Algebra.MonadSpec(spec) where

import Prelude hiding (Monad(..), Functor(..))
import Test.Hspec
import Test.QuickCheck
import Algebra.Base(Monad(..), MonadLaws(..), Functor(..))
import Algebra.Monad

spec :: Spec
spec = do
  spec_left_id
  spec_right_id
  spec_assoc

spec_left_id :: Spec
spec_left_id = describe "Monad Laws" $ do
  it "should verify left identity for List" $
    property (leftId (\x -> [1..x]) :: Int -> Bool)
  it "should verify left identity for Maybe" $
    property (leftId Just :: Int -> Bool)

spec_right_id :: Spec
spec_right_id = describe "Monad Laws" $ do
  it "should verify right id for List" $
    property (rightId :: [Int] -> Bool)
  it "should verify right id for Maybe" $
    property (rightId :: Maybe Int -> Bool)

spec_assoc :: Spec
spec_assoc  = describe "Monad Laws" $ do
  it "should verify associativity for List" $
    property (associativeM (\x -> [1..x]) (\y -> [1..y]):: Int -> Bool)
  it "should verify associativity for Maybe" $
    property (associativeM (\x -> Just (x + 3)) (\y -> Just (7 * y)):: Int -> Bool)
