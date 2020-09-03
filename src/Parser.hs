module Parser
  (
  )
where

import Text.Read (readMaybe)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

parse :: Parser a -> String -> a
parse m s =
  case runParser m s of
    Just (res, []) -> res
    Just (_, rest) -> error "Parser did not consume entire stream"
    Nothing -> error "Parser did not manage to parse anything"

item = Parser $ \s ->
  case s of
    [] -> Nothing
    (c : cs) ->
      let rc = readMaybe [c] :: Maybe Int
       in case rc of
            Just x -> Just (x, cs)
            Nothing -> Nothing