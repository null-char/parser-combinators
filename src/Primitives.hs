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

-- TODO: Implement parsing of floating point numbers
jsonNumber :: Parser JsonVal
jsonNumber = f <$> (satisfy isDigit)
  where
    f s = JsonNumber (read $ T.unpack s) Nothing

charP :: Char -> Parser Char
charP ch = Parser fn
  where
    fn s =
      case T.uncons s of
        Just (c, cs) | c == ch -> Just (c, cs)
        _ -> Nothing

stringP :: T.Text -> Parser [Char]
stringP s = (sequenceA . map charP) $ T.unpack s

jsonNull :: Parser JsonVal
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonVal
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f a = case a of
      "true" -> JsonBool True
      "false" -> JsonBool False

-- A test parser
digit :: Parser Int
digit = Parser $ \s ->
  case T.uncons s of
    Nothing -> Nothing
    Just (c, cs) -> case readMaybe [c] of
      Just x -> Just (x, cs)
      Nothing -> Nothing