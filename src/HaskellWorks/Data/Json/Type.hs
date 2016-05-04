module HaskellWorks.Data.Json.Type where

import HaskellWorks.Data.Positioning

data JsonType
  = JsonTypeArray
  | JsonTypeBool
  | JsonTypeNull
  | JsonTypeNumber
  | JsonTypeObject
  | JsonTypeString

class JsonTypeAt a where
  jsonTypeAtPosition :: Position -> a -> Maybe JsonType
  jsonTypeAt :: a -> Maybe JsonType
