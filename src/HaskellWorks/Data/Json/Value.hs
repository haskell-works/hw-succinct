module HaskellWorks.Data.Json.Value where

import Data.Map

data JsonValue s n
  = JsonString s
  | JsonNumber n
  | JsonObject (Map s (JsonValue s n))
  | JsonArray [JsonValue s n]
  | JsonBool Bool
  | JsonNull
