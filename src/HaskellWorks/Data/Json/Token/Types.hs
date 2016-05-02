module HaskellWorks.Data.Json.Token.Types (JsonToken(..)) where

data JsonToken
  = JsonTokenBraceL
  | JsonTokenBraceR
  | JsonTokenBracketL
  | JsonTokenBracketR
  | JsonTokenComma
  | JsonTokenColon
  | JsonTokenWhitespace
  | JsonTokenString String
  | JsonTokenBoolean Bool
  | JsonTokenNumber Double
  | JsonTokenNull
  deriving (Eq, Show)
