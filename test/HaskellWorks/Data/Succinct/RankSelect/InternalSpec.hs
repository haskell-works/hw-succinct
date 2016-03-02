{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.InternalSpec (spec) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Succinct.RankSelect
import           Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  it "rank True 10010010 over [0..8] should be 011122233" $
    let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
    fmap (rank True bs) [0..8] `shouldBe` [0, 1, 1, 1, 2, 2, 2, 3, 3]
  it "rank True 10010010 over [0..8] should be 001223445" $
    let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
    fmap (rank False bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
  it "select True 10010010 over [0..3] should be 0147" $
    let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
    fmap (select True bs) [0..3] `shouldBe` [0, 1, 4, 7]
  it "select False 10010010 over [0..5] should be 023568" $
    let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
    fmap (select False bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]