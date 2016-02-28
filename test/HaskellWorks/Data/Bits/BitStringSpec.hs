{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module HaskellWorks.Data.Bits.BitStringSpec (spec) where

import qualified Data.Bits as B
import           Data.Int
import qualified Data.Vector as DV
import           Data.Word
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BitStringSpec" $ do
  it "fromBitString \"10000000 101\" :: Maybe [Word8]" $
    let w = fromBitString "10000000 101" :: Maybe [Word8] in
    w `shouldBe` Just [1, 5]
  it "fromBitString \"11100100 10101111 1\" :: Maybe [Word8]" $
    let ws = fromBitString "11100100 10101111 1" :: Maybe [Word8] in
    ws `shouldBe` Just [39, 245, 1]
  it "fromBitString \"\" :: Maybe [Word8]" $
    let ws = fromBitString "" :: Maybe [Word8] in
    ws `shouldBe` Just []
  it "fromBitString \"10000000 101\" :: Maybe (DV.Vector Word8)" $
    let v = fromBitString "10000000 101" :: Maybe (DV.Vector Word8) in
    v `shouldBe` Just (DV.fromList [1, 5])
  it "fromBitString \"11100100 10101111 1\" :: Maybe (DV.Vector Word8)" $
    let v = fromBitString "11100100 10101111 1" :: Maybe (DV.Vector Word8) in
    v `shouldBe` Just (DV.fromList [39, 245, 1])
  it "fromBitString \"\" :: Maybe (DV.Vector Word8)" $
    let v = fromBitString "" :: Maybe (DV.Vector Word8) in
    v `shouldBe` Just (DV.fromList [])
  it "bitPrint (8 :: Word8)" $
    let bs = toBitString (8 :: Word8) in
    bs `shouldBe` "00010000"
  it "bitPrint (8 :: Word64)" $
    let bs = toBitString (8 :: Word64) in
    bs `shouldBe` "00010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "bitPrint [0x0102040810204080 :: Word64]" $
    let bs = toBitString [0x0102040810204080 :: Word64] in
    bs `shouldBe` "00000001 00000010 00000100 00001000 00010000 00100000 01000000 10000000"