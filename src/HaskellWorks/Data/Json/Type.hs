module HaskellWorks.Data.Json.Type where

data JsonType
  = JsonTypeArray
  | JsonTypeBool
  | JsonTypeNull
  | JsonTypeNumber
  | JsonTypeObject
  | JsonTypeString

class JsonTypeAt a where
  jsonTypeAt :: a -> Maybe JsonType
