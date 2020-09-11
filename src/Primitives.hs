{-# LANGUAGE OverloadedStrings #-}

module Primitives (jsonVal) where

import Control.Applicative
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.Text as T
import Parser
import Text.Read (readMaybe)

data JsonVal
  = -- Maybe indicates the fractional part of the number. This will obviously be `Nothing` if it's
    -- just a plain integer.
    JsonNumber Integer (Maybe Double)
  | JsonBool Bool
  | JsonNull
  | JsonString T.Text
  | JsonArray [JsonVal]
  | JsonObj (M.Map T.Text JsonVal)
  deriving (Eq, Show)

-- | The main JSON value parser capable of parsing every valid JSON value
jsonVal :: Parser JsonVal
jsonVal = jsonNumber <|> jsonNull <|> jsonBool <|> jsonString <|> jsonArray <|> jsonObj

-- | Creates a parser that parses pretty much any list of characters that satisfy the given predicate
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

-- | Parses JSON numbers
--
-- Supports floating point numbers and negative numbers.
jsonNumber :: Parser JsonVal
jsonNumber = f <$> intPart <*> fractPart
  where
    -- We check to see if the passed in JSON number is negative. If not, we alternate to just a Parser
    -- that parses positive integers.
    intPart = ((\a b -> T.pack $ a : T.unpack b) <$> charP '-' <*> satisfy isDigit) <|> satisfy isDigit
    -- This parser will TRY to parse the fractional part of the provided JSON number. If the attempt
    -- yielded `Nothing` (meaning there is no fractional part) it will instead default to a pure empty
    -- text value.
    fractPart = (charP '.' *> satisfy isDigit) <|> pure T.empty
    -- We have to prepend the fractional part with "0." so that the double is parsed correctly
    f int fr = JsonNumber (read $ T.unpack int) ((readMaybe $ "0." ++ T.unpack fr) :: Maybe Double)

-- | Parses almost all characters including escaped ones excluding plain quotation marks (not escaped)
stringVal :: Parser T.Text
stringVal = T.pack <$> many (escapeChar <|> basicChar)

-- | Parses JSON strings (no support for escaping yet)
jsonString :: Parser JsonVal
jsonString =
  JsonString <$> (charP '"' *> stringVal <* charP '"')

-- | Parses whitespace characters
ws :: Parser Char
ws = parseCharIf (== ' ')

-- | A parser that parses comma separators
--
-- A basic example would be something like: ", " or " ,  "
commaSep :: Parser Char
commaSep = many ws *> charP ',' <* many ws

-- | Parses JSON arrays
jsonArray :: Parser JsonVal
jsonArray =
  JsonArray
    <$> (charP '[' *> many ws *> values <* many ws <* charP ']')
  where
    values = (:) <$> (elem) <*> many (commaSep *> many ws *> elem)
    elem = jsonVal <* many ws

-- | Parses JSON objects
jsonObj :: Parser JsonVal
jsonObj =
  -- The Map is created using the `fromList` function which essentially just takes a list of key value
  -- pairs and constructs a map from it.
  JsonObj
    <$> (M.fromList <$> (charP '{' *> many ws *> (keyValuePairs <|> pure []) <* many ws <* charP '}'))
  where
    -- A `pair` simply just denotes a single key value pair. eg: `(key, value)`
    pair =
      (\k _ v -> (k, v)) <$> (charP '"' *> stringVal <* charP '"') <*> (many ws *> charP ':' <* many ws) <*> jsonVal
    -- `keyValuePairs` represents a list of key value pairs. eg: `[(k1, v1), (k2, v2)]`
    keyValuePairs = (:) <$> pair <*> many (commaSep *> many ws *> pair)

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

-- | Creates a parser that can only parse the character that the creator function is provided with
charP :: Char -> Parser Char
charP ch = parseCharIf (== ch)

-- | Creates a parser than only parses the string that the creator function is provided with
stringP :: T.Text -> Parser T.Text
stringP s = T.pack <$> ((sequenceA . map charP) $ T.unpack s)

-- | Parses null values
jsonNull :: Parser JsonVal
jsonNull = (\_ -> JsonNull) <$> stringP "null"

-- | Parses boolean values
jsonBool :: Parser JsonVal
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f a = case a of
      "true" -> JsonBool True
      "false" -> JsonBool False
