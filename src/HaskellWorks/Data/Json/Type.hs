module HaskellWorks.Data.Json.Type where

data JsonType
  = JsonTypeString
  | JsonTypeNumber
  | JsonTypeObject
  | JsonTypeArray
  | JsonTypeBool
  | JsonTypeNul

class JsonTypeAt a where
  jsonTypeAt :: a -> JsonType
