module HaskellWorks.Data.Json.Value where

import Data.Map

data GenJsonValue s n
  = JsonString s
  | JsonNumber n
  | JsonObject (Map s (GenJsonValue s n))
  | JsonArray [GenJsonValue s n]
  | JsonBool
  | JsonNull
