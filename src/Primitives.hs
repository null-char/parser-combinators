{-# LANGUAGE OverloadedStrings #-}

module Primitives () where

import Control.Applicative
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.Text as T
import Parser
import Text.Read (readMaybe)

data JsonVal
  = -- Maybe indicates the fractional part of the number. This will obviously be `Nothing` if it's
    -- just a plain integer.
    JsonNumber Integer (Maybe Integer)
  | JsonBool Bool
  | JsonNull
  | JsonString T.Text
  | JsonArray [JsonVal]
  | JsonObj (M.Map T.Text JsonVal)
  deriving (Eq, Show)

jsonVal :: Parser JsonVal
jsonVal = jsonNumber <|> jsonNull <|> jsonBool

satisfy :: (Char -> Bool) -> Parser T.Text
satisfy predicate = Parser $ \s ->
  let (t, rest) = span predicate $ T.unpack s
   in -- If we don't manage to extract any valid tokens, then the received text will be null
      -- If so, we'll fail the parsing
      case t of
        [] -> Nothing
        _ -> Just (T.pack t, T.pack rest)

-- | Creates a parser that parses a character if and only if the provided predicate holds
--
-- This can be utilized as a building block for creating `Parser Char`s with different requirements
parseCharIf :: (Char -> Bool) -> Parser Char
parseCharIf predicate = Parser $ \s ->
  case T.uncons s of
    Just (c, cs) | predicate c -> Just (c, cs)
    _ -> Nothing

-- TODO: Implement parsing of floating point numbers
jsonNumber :: Parser JsonVal
jsonNumber = f <$> (satisfy isDigit)
  where
    f s = JsonNumber (read $ T.unpack s) Nothing

literal :: Parser T.Text
literal = satisfy (/= '"')

-- | Parses JSON strings (no support for escaping yet)
jsonString :: Parser JsonVal
jsonString =
  JsonString <$> (charP '"' *> (T.pack <$> many (escapeChar <|> basicChar)) <* charP '"')

-- | Parses escape characters
--
-- NOTE: No support for unicode
escapeChar :: Parser Char
escapeChar =
  ('\\' <$ stringP "\\\\")
    <|> ('"' <$ stringP "\\\"") -- The `<$` function essentially partially injects the left value to
    <|> ('\n' <$ stringP "\\n") -- to the right Functor. The remaining stream is not advanced, the value
    <|> ('\b' <$ stringP "\\b") -- is simply reinterpreted.
    <|> ('/' <$ stringP "\\/")
    <|> ('\f' <$ stringP "\\f")
    <|> ('\r' <$ stringP "\\r")
    <|> ('\t' <$ stringP "\\t")

-- | Parses all basic characters except backslash and quotes
basicChar :: Parser Char
basicChar = parseCharIf (\c -> (c /= '"' && c /= '\\'))

charP :: Char -> Parser Char
charP ch = parseCharIf (== ch)

stringP :: T.Text -> Parser T.Text
stringP s = T.pack <$> ((sequenceA . map charP) $ T.unpack s)

jsonNull :: Parser JsonVal
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonVal
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f a = case a of
      "true" -> JsonBool True
      "false" -> JsonBool False
