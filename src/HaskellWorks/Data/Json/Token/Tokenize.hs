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

parseJsonTokenString :: (P.Parser t, Alternative (T.Parser t), IsString t) => T.Parser t JsonToken
parseJsonTokenString = do
  _ <- string "\""
  value <- many (verbatimChar <|> escapedChar <|> escapedCode)
  _ <- string "\""
  return $ JsonTokenString value

parseJsonTokenBraceL :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenBraceL = string "{" >> return JsonTokenBraceL

parseJsonTokenBraceR :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenBraceR = string "}" >> return JsonTokenBraceR

parseJsonTokenBracketL :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenBracketL = string "[" >> return JsonTokenBracketL

parseJsonTokenBracketR :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenBracketR = string "]" >> return JsonTokenBracketR

parseJsonTokenComma :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenComma = string "," >> return JsonTokenComma

parseJsonTokenColon :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenColon = string ":" >> return JsonTokenColon

parseJsonTokenWhitespace :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenWhitespace = do
  _ <- AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
  return JsonTokenWhitespace

parseJsonTokenNull :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenNull = string "null" >> return JsonTokenNull

parseJsonTokenBoolean :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenBoolean = true <|> false
  where
    true  = string "true"   >> return (JsonTokenBoolean True)
    false = string "false"  >> return (JsonTokenBoolean False)

parseJsonTokenDouble :: (P.Parser t, IsString t) => T.Parser t JsonToken
parseJsonTokenDouble = JsonTokenNumber <$> rational

parseJsonToken :: (P.Parser t, IsString t) => T.Parser t JsonToken
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
