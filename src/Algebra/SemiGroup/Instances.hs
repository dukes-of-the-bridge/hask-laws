{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Algebra.SemiGroup.Instances(Sum(..), Product(..)) where
import GHC.Generics

newtype Sum a = Sum { getSum:: a} 
                deriving ( Eq    
                         , Ord
                         , Read
                         , Show
                         , Bounded
                         , Num
                         , Generic)

newtype Product a = Product { getProduct:: a}
                    deriving ( Eq
                             , Ord
                             , Read
                             , Show
                             , Bounded
                             , Num
                             , Generic)
