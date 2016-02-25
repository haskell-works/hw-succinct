{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.BitWise
    ( -- * Bit map
      BitLength(..)
    , BitWise(..)
    , PopCount(..)
    , Shift(..)
    , TestBit(..)
    ) where

import qualified Data.Bits                              as B
import           Data.Int
import           Data.Word
import           HaskellWorks.Data.Positioning

-- We pervasively use precedence to avoid excessive parentheses, and we use
-- the same precedence conventions of the C programming language: arithmetic
-- operators come first, ordered in the standard way, followed by shifts,
-- followed by logical operators; ⊕ sits between | and &.
infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.        -- Bitwise AND.  eg. ∧
infixl 6 .^.        -- Bitwise XOR.  eg. ⊕
infixl 5 .|.        -- Bitwise OR.   eg. ∨

class BitLength v where
  bitLength :: v -> Count

  endPosition :: v -> Position
  endPosition = Position . fromIntegral . getCount . bitLength

class Shift a where
  (.<.) :: a -> Int64 -> a
  (.>.) :: a -> Int64 -> a

class TestBit a where
  (.?.) :: a -> Position -> Bool

class BitWise a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  (.^.) :: a -> a -> a
  comp  :: a -> a

class PopCount a where
  popCount :: a -> Count

--------------------------------------------------------------------------------
-- Instances

instance BitLength Word8 where
  bitLength _ = 8
  {-# INLINABLE bitLength #-}

instance BitLength Word16 where
  bitLength _ = 16
  {-# INLINABLE bitLength #-}

instance BitLength Word32 where
  bitLength _ = 32
  {-# INLINABLE bitLength #-}

instance BitLength Word64 where
  bitLength _ = 64
  {-# INLINABLE bitLength #-}

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance PopCount Word8 where
  popCount x0 = Count (fromIntegral x3)
    where
      x1 = x0 - ((x0 .&. 0xaa) .>. 1)
      x2 = (x1 .&. 0x33) + ((x1 .>. 2) .&. 0x33)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f
  {-# INLINABLE popCount #-}

instance PopCount Word16 where
  popCount x0 = Count (fromIntegral ((x3 * 0x0101) .>. 8))
    where
      x1 = x0 - ((x0 .&. 0xaaaa) .>. 1)
      x2 = (x1 .&. 0x3333) + ((x1 .>. 2) .&. 0x3333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f
  {-# INLINABLE popCount #-}

instance PopCount Word32 where
  popCount x0 = Count (fromIntegral ((x3 * 0x01010101) .>. 24))
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x33333333) + ((x1 .>. 2) .&. 0x33333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f
  {-# INLINABLE popCount #-}

instance PopCount Word64 where
  popCount x0 = Count ((x3 * 0x0101010101010101) .>. 56)
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaaaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x3333333333333333) + ((x1 .>. 2) .&. 0x3333333333333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f
  {-# INLINABLE popCount #-}

instance BitWise Word8 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance BitWise Word16 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance BitWise Word32 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance BitWise Word64 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}
