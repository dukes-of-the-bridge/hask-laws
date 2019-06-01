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
spec_left_id = describe "Monad laws" $
  it "should verify left identity for lists" $
    property (leftId (\x -> [1..x]) :: Int -> Bool)

spec_right_id :: Spec
spec_right_id = describe "Monad laws" $
  it "should verify right id for lists" $
    property (rightId :: [Int] -> Bool)

spec_assoc :: Spec
spec_assoc  = describe "Monad laws" $
  it "should verify associativity for lists" $
    property (associativeM (\x -> [1..x]) (\y -> [1..y]):: Int -> Bool)
