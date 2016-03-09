{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitWiseSpec (spec) where

import qualified Data.Bits                        as B
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitWise
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  describe "for popCount0" $ do
    it "for Word8 matches Data.Bits implementation" $ property $
      \(w :: Word8 ) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for Word16 matches Data.Bits implementation" $ property $
      \(w :: Word16) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for Word32 matches Data.Bits implementation" $ property $
      \(w :: Word32) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for Word64 matches Data.Bits implementation" $ property $
      \(w :: Word64) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for [Word8] matches Data.Bits implementation" $ property $
      \(w :: [Word8] ) -> popCount0 w == bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word16] matches Data.Bits implementation" $ property $
      \(w :: [Word16]) -> popCount0 w == bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word32] matches Data.Bits implementation" $ property $
      \(w :: [Word32]) -> popCount0 w == bitLength w - sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word64] matches Data.Bits implementation" $ property $
      \(w :: [Word64]) -> popCount0 w == bitLength w - sum (fmap (fromIntegral . B.popCount) w)
  describe "for popCount1" $ do
    it "for Word8 matches Data.Bits implementation" $ property $
      \(w :: Word8 ) -> popCount1 w == fromIntegral (B.popCount w)
    it "for Word16 matches Data.Bits implementation" $ property $
      \(w :: Word16) -> popCount1 w == fromIntegral (B.popCount w)
    it "for Word32 matches Data.Bits implementation" $ property $
      \(w :: Word32) -> popCount1 w == fromIntegral (B.popCount w)
    it "for Word64 matches Data.Bits implementation" $ property $
      \(w :: Word64) -> popCount1 w == fromIntegral (B.popCount w)
    it "for [Word8] matches Data.Bits implementation" $ property $
      \(w :: [Word8] ) -> popCount1 w == sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word16] matches Data.Bits implementation" $ property $
      \(w :: [Word16]) -> popCount1 w == sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word32] matches Data.Bits implementation" $ property $
      \(w :: [Word32]) -> popCount1 w == sum (fmap (fromIntegral . B.popCount) w)
    it "for [Word64] matches Data.Bits implementation" $ property $
      \(w :: [Word64]) -> popCount1 w == sum (fmap (fromIntegral . B.popCount) w)
