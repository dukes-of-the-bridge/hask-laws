{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Algebra.SemiGroup.Instances(Sum(..), Product(..)) where

newtype Sum a = Sum { getSum:: a} 
                deriving ( Eq    
                         , Ord
                         , Read
                         , Show
                         , Bounded
                         , Num )

newtype Product a = Product { getProduct:: a}
                    deriving ( Eq
                             , Ord
                             , Read
                             , Show
                             , Bounded
                             , Num )
