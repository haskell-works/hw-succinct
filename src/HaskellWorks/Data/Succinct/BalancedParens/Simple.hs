{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.BalancedParens.Simple
  ( SimpleBalancedParens(..)
  , closeAt
  , findOpen
  , findClose
  , findClose'
  , openAt
  ) where

import           Data.Typeable
import           Debug.Trace
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitPrint
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           Prelude                                                    as P

newtype SimpleBalancedParens a = SimpleBalancedParens a
  deriving (BitLength, Eq, BitPrint, TestBit, Rank0, Rank1, Select0, Select1)

instance Functor SimpleBalancedParens where
  fmap f (SimpleBalancedParens a) = SimpleBalancedParens (f a)

instance (BitPrint a, ToBitString a) => Show (SimpleBalancedParens a) where
  show = toBitString

closeAt :: TestBit a => a -> Count -> Bool
closeAt v c = not (v .?. lastPositionOf c)

openAt :: TestBit a => a -> Count -> Bool
openAt v c = v .?. lastPositionOf c

require :: Bool -> String -> a -> a
require p msg v = if p then v else error msg

findOpen' :: (BitLength a, TestBit a) => Count -> SimpleBalancedParens a -> Count -> Count
findOpen' c v p =
  require (0 < p && p <= bitLength v) "Out of bounds" $
  if v `openAt` p
    then if c == 0
      then p
      else findOpen' (c - 1) v (p - 1)
    else findOpen' (c + 1) v (p - 1)

findClose' :: (BitLength a, TestBit a) => Count -> SimpleBalancedParens a -> Count -> Count
findClose' c v p =
  require (1 < p && p <= bitLength v) "Out of bounds" $
  if v `closeAt` p
    then if c == 0
      then p
      else findClose' (c + 1) v (p + 1)
    else findClose' (c - 1) v (p + 1)

instance (Typeable a, BitPrint a, BitLength a, TestBit a) => BalancedParens (SimpleBalancedParens a) where
  findOpen  v p = if v `openAt`  p then p else findOpen'  (Count 0) v (p - 1)
  findClose v p = let x = if v `closeAt` p then p else findClose' (Count 0) v (p + 1) in trace ("findClose " ++ show v ++ " " ++ show p ++ " => " ++ show x) x
  enclose       = findOpen' (Count 1)
