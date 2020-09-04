{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( runParser,
    Parser (..),
  )
where

import Control.Applicative
import qualified Data.Text as T

-- | Wraps a `parse` function into a newtype called `Parser` parameterized by some type `a`.
newtype Parser a = Parser {parse :: T.Text -> Maybe (a, T.Text)}

-- Lawful instance of Functor implemented for Parser
instance Functor Parser where
  -- Applies a function to the parsed stream yielding another Parser with the transformed stream
  fmap f p = Parser $ \s -> do
    (x, rest) <- parse p s
    Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)

  -- This is how we'll essentially end up chaining parsers together
  (<*>) (Parser p1) (Parser p2) = Parser $ \s -> do
    (f, s') <- p1 s
    (x, s'') <- p2 s'
    Just (f x, s'')

instance Alternative Parser where
  -- Picks the first non-empty `Maybe`
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s

  -- The "empty" value for Parser will be a parser that parses to `Nothing` which indicates a failure
  empty = Parser $ \_ -> Nothing

runParser :: Parser a -> T.Text -> a
runParser m s =
  case parse m s of
    Just (res, "") -> res
    Just (_, rest) -> error "Parser did not consume the entire stream"
    Nothing -> error "Parser did not manage to parse anything"
