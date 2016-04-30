{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.Internal
  ( JsonCursor(..)
  ) where

import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Char8                                 as BSC
import           Data.ByteString.Internal                              as BSI
import           Data.String
import qualified Data.Vector.Storable                                  as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor.BalancedParens
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens             as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import           HaskellWorks.Data.TreeCursor

data JsonCursor t v w = JsonCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Show)

instance  (FromBlankedJson (JsonInterestBits a), FromBlankedJson (JsonBalancedParens b))
          => FromByteString (JsonCursor BS.ByteString a b) where
  fromByteString bs   = JsonCursor
    { cursorText      = bs
    , interests       = getJsonInterestBits (fromBlankedJson blankedJson)
    , balancedParens  = getJsonBalancedParens (fromBlankedJson blankedJson)
    , cursorRank      = 1
    }
    where blankedJson :: BlankedJson
          blankedJson = fromByteString bs

instance IsString (JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])) where
  fromString :: String -> JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank      = 1
    , interests       = getJsonInterestBits (fromBlankedJson blankedJson)
    , balancedParens  = getJsonBalancedParens (fromBlankedJson blankedJson)
    }
    where blankedJson :: BlankedJson
          blankedJson = fromByteString (BSC.pack s)

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (JsonCursor t v u) where
  firstChild :: JsonCursor t v u -> JsonCursor t v u
  firstChild k = k { cursorRank = BP.firstChild (balancedParens k) (cursorRank k) }

  nextSibling :: JsonCursor t v u -> JsonCursor t v u
  nextSibling k = k { cursorRank = BP.nextSibling (balancedParens k) (cursorRank k) }

  parent :: JsonCursor t v u -> JsonCursor t v u
  parent k = k { cursorRank = BP.parent (balancedParens k) (cursorRank k) }

  depth :: JsonCursor t v u -> Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: JsonCursor t v u -> Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)
