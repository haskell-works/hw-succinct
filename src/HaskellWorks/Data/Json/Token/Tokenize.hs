{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Token.Tokenize
    ( IsChar(..)
    , JsonToken(..)
    , parseJsonToken
    , parseJsonTokenString
    , escapedChar
    ) where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8   as BC
import qualified Data.Attoparsec.Combinator         as AC
import qualified Data.Attoparsec.Types              as T
import qualified Data.ByteString                    as BS
import           Data.Bits
import           Data.Char
import           Data.String
import           HaskellWorks.Data.Parser           as P
import           HaskellWorks.Data.Char.IsChar
import           HaskellWorks.Data.Json.Token.Types

hexDigitNumeric :: P.Parser t => T.Parser t Int
hexDigitNumeric = do
  c <- satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

hexDigitAlphaLower :: P.Parser t => T.Parser t Int
hexDigitAlphaLower = do
  c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

hexDigitAlphaUpper :: P.Parser t => T.Parser t Int
hexDigitAlphaUpper = do
  c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

hexDigit :: P.Parser t => T.Parser t Int
hexDigit = hexDigitNumeric <|> hexDigitAlphaLower <|> hexDigitAlphaUpper

verbatimChar :: P.Parser t => T.Parser t Char
verbatimChar  = satisfyChar (BC.notInClass "\"\\") <?> "invalid string character"

escapedChar :: (IsString t, P.Parser t) => T.Parser t Char
escapedChar   = do
  _ <- string "\\"
  (   char '"'  >> return '"'  ) <|>
    ( char 'b'  >> return '\b' ) <|>
    ( char 'n'  >> return '\n' ) <|>
    ( char 'f'  >> return '\f' ) <|>
    ( char 'r'  >> return '\r' ) <|>
    ( char 't'  >> return '\t' ) <|>
    ( char '\\' >> return '\\' ) <|>
    ( char '\'' >> return '\'' ) <|>
    ( char '/'  >> return '/'  )

escapedCode :: (IsString t, P.Parser t) => T.Parser t Char
escapedCode   = do
  _ <- string "\\u"
  a <- hexDigit
  b <- hexDigit
  c <- hexDigit
  d <- hexDigit
  return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

class ParseJson t where
  parseJsonTokenString :: T.Parser t (JsonToken String Double)
  parseJsonToken :: T.Parser t (JsonToken String Double)
  parseJsonTokenBraceL :: T.Parser t (JsonToken String Double)
  parseJsonTokenBraceR :: T.Parser t (JsonToken String Double)
  parseJsonTokenBracketL :: T.Parser t (JsonToken String Double)
  parseJsonTokenBracketR :: T.Parser t (JsonToken String Double)
  parseJsonTokenComma :: T.Parser t (JsonToken String Double)
  parseJsonTokenColon :: T.Parser t (JsonToken String Double)
  parseJsonTokenWhitespace :: T.Parser t (JsonToken String Double)
  parseJsonTokenNull :: T.Parser t (JsonToken String Double)
  parseJsonTokenBoolean :: T.Parser t (JsonToken String Double)
  parseJsonTokenDouble :: T.Parser t (JsonToken String Double)
  parseJsonToken =
    parseJsonTokenString     <|>
    parseJsonTokenBraceL     <|>
    parseJsonTokenBraceR     <|>
    parseJsonTokenBracketL   <|>
    parseJsonTokenBracketR   <|>
    parseJsonTokenComma      <|>
    parseJsonTokenColon      <|>
    parseJsonTokenWhitespace <|>
    parseJsonTokenNull       <|>
    parseJsonTokenBoolean    <|>
    parseJsonTokenDouble

instance ParseJson BS.ByteString where
  parseJsonTokenBraceL = string "{" >> return JsonTokenBraceL
  parseJsonTokenBraceR = string "}" >> return JsonTokenBraceR
  parseJsonTokenBracketL = string "[" >> return JsonTokenBracketL
  parseJsonTokenBracketR = string "]" >> return JsonTokenBracketR
  parseJsonTokenComma = string "," >> return JsonTokenComma
  parseJsonTokenColon = string ":" >> return JsonTokenColon
  parseJsonTokenNull = string "null" >> return JsonTokenNull
  parseJsonTokenDouble = JsonTokenNumber <$> rational

  parseJsonTokenString = do
    _ <- string "\""
    value <- many (verbatimChar <|> escapedChar <|> escapedCode)
    _ <- string "\""
    return $ JsonTokenString value

  parseJsonTokenWhitespace = do
    _ <- AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
    return JsonTokenWhitespace

  parseJsonTokenBoolean = true <|> false
    where
      true  = string "true"   >> return (JsonTokenBoolean True)
      false = string "false"  >> return (JsonTokenBoolean False)
