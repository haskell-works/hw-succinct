{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor
  ( module X
  , jsonTokenAt
  , jsonTokenValid
  ) where

import qualified Data.Attoparsec.ByteString.Char8                           as ABC
import           Data.ByteString.Internal                                   as BSI
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Json.Final.Tokenize.Internal
import           HaskellWorks.Data.Json.Succinct.Cursor.CursorType          as X
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal            as X
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

jsonTokenValid :: (Rank1 w, Select1 v, TestBit w) => JsonCursor ByteString v w -> Bool
jsonTokenValid cursor = balancedParens cursor .?. lastPositionOf (cursorRank cursor)

jsonTokenAt :: (Rank1 w, Select1 v) => JsonCursor ByteString v w -> JsonToken
jsonTokenAt k = case ABC.parse parseJsonToken (vDrop (toCount (jsonCursorPos k)) (cursorText k)) of
  ABC.Fail    {}  -> error "Failed to parse token in cursor"
  ABC.Partial _   -> error "Failed to parse token in cursor"
  ABC.Done    _ r -> r
