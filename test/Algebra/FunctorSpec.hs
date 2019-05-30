module Algebra.FunctorSpec(spec) where

import Prelude hiding (Functor(..))
import Test.Hspec
import Test.QuickCheck
import Algebra.Base(Functor(..), FunctorLaw(..))
import Algebra.Functor


spec::Spec
spec = do
  spec_map_id
  spec_composition

spec_map_id :: Spec
spec_map_id  = describe "Functor" $ do
   it "maps identity function for Lists" $
     property (mapId :: [Int] -> Bool)
   it "maps identity function for Maybe" $
     property (mapId :: Maybe Int -> Bool)

spec_composition::Spec
spec_composition = describe "Functor" $ do
  it "map composition for lists" $
    property (mapCompose bySeven show :: [Int] -> Bool)
  it "map composition for lists" $
    property (mapCompose bySeven show :: Maybe Int -> Bool)


bySeven :: (Num n) => n -> n
bySeven = (*) 7
