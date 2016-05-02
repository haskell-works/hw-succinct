{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module HaskellWorks.Data.Json.Token.Tokenize
    ( IsChar(..)
    , JsonToken(..)
    , ParseJson(..)
    ) where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8   as BC
import qualified Data.Attoparsec.Combinator         as AC
import qualified Data.Attoparsec.Types              as T
import qualified Data.ByteString                    as BS
import           Data.Bits
import           Data.Char
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

class ParseJson t s d where
  parseJsonTokenString :: T.Parser t (JsonToken s d)
  parseJsonToken :: T.Parser t (JsonToken s d)
  parseJsonTokenBraceL :: T.Parser t (JsonToken s d)
  parseJsonTokenBraceR :: T.Parser t (JsonToken s d)
  parseJsonTokenBracketL :: T.Parser t (JsonToken s d)
  parseJsonTokenBracketR :: T.Parser t (JsonToken s d)
  parseJsonTokenComma :: T.Parser t (JsonToken s d)
  parseJsonTokenColon :: T.Parser t (JsonToken s d)
  parseJsonTokenWhitespace :: T.Parser t (JsonToken s d)
  parseJsonTokenNull :: T.Parser t (JsonToken s d)
  parseJsonTokenBoolean :: T.Parser t (JsonToken s d)
  parseJsonTokenDouble :: T.Parser t (JsonToken s d)

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

instance ParseJson BS.ByteString String Double where
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
    where
      verbatimChar  = satisfyChar (BC.notInClass "\"\\") <?> "invalid string character"
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
      escapedCode   = do
        _ <- string "\\u"
        a <- hexDigit
        b <- hexDigit
        c <- hexDigit
        d <- hexDigit
        return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

  parseJsonTokenWhitespace = do
    _ <- AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
    return JsonTokenWhitespace

  parseJsonTokenBoolean = true <|> false
    where
      true  = string "true"   >> return (JsonTokenBoolean True)
      false = string "false"  >> return (JsonTokenBoolean False)