{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module HaskellWorks.Data.Conduit.Json
  ( blankedJsonToInterestBits
  , byteStringToBits
  , blankedJsonToBalancedParens
  , blankedJsonToBalancedParens2
  , compressWordAsBit
  , interestingWord8s
  ) where

import           Control.Monad
import           Data.Array.Unboxed                   as A
import qualified Data.Bits                            as BITS
import           Data.ByteString                      as BS
import           Data.Conduit
import           Data.Int
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Conduit.Json.Words
import           Prelude                              as P

interestingWord8s :: A.UArray Word8 Word8
interestingWord8s = A.array (0, 255) [
  (w, if w == wOpenBracket || w == wOpenBrace || w == wOpenParen || w == wt || w == wf || w == wn || w == w1
    then 1
    else 0)
  | w <- [0 .. 255]]

blankedJsonToInterestBits :: Monad m => Conduit BS.ByteString m BS.ByteString
blankedJsonToInterestBits = blankedJsonToInterestBits' ""

padRight :: Word8 -> Int -> BS.ByteString -> BS.ByteString
padRight w n bs = if BS.length bs >= n then bs else fst (BS.unfoldrN n gen bs)
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen cs = case BS.uncons cs of
          Just (c, ds) -> Just (c, ds)
          Nothing      -> Just (w, BS.empty)

blankedJsonToInterestBits' :: Monad m => BS.ByteString -> Conduit BS.ByteString m BS.ByteString
blankedJsonToInterestBits' rs = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = if BS.length rs /= 0 then BS.concat [rs, bs] else bs
      let lencs = BS.length cs
      let q = lencs + 7 `quot` 8
      let (ds, es) = BS.splitAt (q * 8) cs
      let (fs, _) = BS.unfoldrN q gen ds
      yield fs
      blankedJsonToInterestBits' es
    Nothing -> return ()
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen as = if BS.length as == 0
          then Nothing
          else Just ( BS.foldr (\b m -> (interestingWord8s ! b) .|. (m .<. 1)) 0 (padRight 0 8 (BS.take 8 as))
                    , BS.drop 8 as
                    )

blankedJsonToBalancedParens :: Monad m => Conduit BS.ByteString m Bool
blankedJsonToBalancedParens = do
  mbs <- await
  case mbs of
    Just bs -> blankedJsonToBalancedParens' bs
    Nothing -> return ()

blankedJsonToBalancedParens' :: Monad m => BS.ByteString -> Conduit BS.ByteString m Bool
blankedJsonToBalancedParens' bs = case BS.uncons bs of
  Just (c, cs) -> do
    case c of
      d | d == wOpenBrace     -> yield True
      d | d == wCloseBrace    -> yield False
      d | d == wOpenBracket   -> yield True
      d | d == wCloseBracket  -> yield False
      d | d == wOpenParen     -> yield True
      d | d == wCloseParen    -> yield False
      d | d == wt             -> yield True >> yield False
      d | d == wf             -> yield True >> yield False
      d | d == w1             -> yield True >> yield False
      d | d == wn             -> yield True >> yield False
      _                       -> return ()
    blankedJsonToBalancedParens' cs
  Nothing -> return ()

compressWordAsBit :: Monad m => Conduit BS.ByteString m BS.ByteString
compressWordAsBit = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (cs, _) = BS.unfoldrN (BS.length bs + 7 `div` 8) gen bs
      yield cs
    Nothing -> return ()
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen xs = if BS.length xs == 0
          then Nothing
          else Just ( BS.foldr (\b m -> ((b .&. 1) .|. (m .<. 1))) 0 (padRight 0 8 (BS.take 8 xs))
                    , BS.drop 8 xs
                    )

blankedJsonToBalancedParens2 :: Monad m => Conduit BS.ByteString m BS.ByteString
blankedJsonToBalancedParens2 = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (cs, _) = BS.unfoldrN (BS.length bs * 2) gen (Nothing, bs)
      yield cs
    Nothing -> return ()
  where gen :: (Maybe Bool, ByteString) -> Maybe (Word8, (Maybe Bool, ByteString))
        gen (Just True  , bs) = Just (wFF, (Nothing, bs))
        gen (Just False , bs) = Just (w00, (Nothing, bs))
        gen (Nothing    , bs) = case BS.uncons bs of
          Just (c, cs) -> case balancedParensOf c of
            MiniN   -> gen        (Nothing    , cs)
            MiniT   -> Just (wFF, (Nothing    , cs))
            MiniF   -> Just (w00, (Nothing    , cs))
            MiniTF  -> Just (wFF, (Just False , cs))
          Nothing   -> Nothing

data MiniBP = MiniN | MiniT | MiniF | MiniTF

balancedParensOf :: Word8 -> MiniBP
balancedParensOf c = case c of
    d | d == wOpenBrace     -> MiniT
    d | d == wCloseBrace    -> MiniF
    d | d == wOpenBracket   -> MiniT
    d | d == wCloseBracket  -> MiniF
    d | d == wOpenParen     -> MiniT
    d | d == wCloseParen    -> MiniF
    d | d == wt             -> MiniTF
    d | d == wf             -> MiniTF
    d | d == w1             -> MiniTF
    d | d == wn             -> MiniTF
    _                       -> MiniN

yieldBitsOfWord8 :: Monad m => Word8 -> Conduit BS.ByteString m Bool
yieldBitsOfWord8 w = do
  yield ((w .&. BITS.bit 0) /= 0)
  yield ((w .&. BITS.bit 1) /= 0)
  yield ((w .&. BITS.bit 2) /= 0)
  yield ((w .&. BITS.bit 3) /= 0)
  yield ((w .&. BITS.bit 4) /= 0)
  yield ((w .&. BITS.bit 5) /= 0)
  yield ((w .&. BITS.bit 6) /= 0)
  yield ((w .&. BITS.bit 7) /= 0)

yieldBitsofWord8s :: Monad m => [Word8] -> Conduit BS.ByteString m Bool
yieldBitsofWord8s = P.foldr ((>>) . yieldBitsOfWord8) (return ())

byteStringToBits :: Monad m => Conduit BS.ByteString m Bool
byteStringToBits = do
  mbs <- await
  case mbs of
    Just bs -> yieldBitsofWord8s (BS.unpack bs) >> byteStringToBits
    Nothing -> return ()
