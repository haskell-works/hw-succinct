{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.Positioning where

import Data.Int
import Data.Word
import Test.QuickCheck as QuickCheck

newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Count where
    show (Count w64) = show w64

instance Arbitrary Count where
    arbitrary = fmap Count arbitrary

newtype Position = Position { getPosition :: Int64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Position where
    show (Position n) = show n

arbitraryNonNegative :: QuickCheck.Gen Int64
arbitraryNonNegative = fmap QuickCheck.getNonNegative arbitrary

instance Arbitrary Position where
    arbitrary = fmap Position arbitraryNonNegative