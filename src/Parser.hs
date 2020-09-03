{-# LANGUAGE OverloadedStrings #-}

module Parser
  (
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
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

newtype Parser a = Parser {parse :: T.Text -> Maybe (a, T.Text)}

runParser :: Parser a -> T.Text -> a
runParser m s =
  case parse m s of
    Just (res, "") -> res
    Just (_, rest) -> error "Parser did not consume the entire stream"
    Nothing -> error "Parser did not manage to parse anything"

-- A test parser
digit :: Parser Int
digit = Parser $ \s ->
  case s of
    "" -> Nothing
    t ->
      let ch = T.head t; cs = T.tail t
       in case readMaybe [ch] of
            Just x -> Just (x, cs)
            Nothing -> Nothing