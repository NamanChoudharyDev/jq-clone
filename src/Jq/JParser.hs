module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Parsing.Utils (rawNumber, escapedString)

parseJNull :: Parser JSON
parseJNull = JNull <$ string "null"

parseJString :: Parser JSON
parseJString = JString <$> escapedString

parseJNumber :: Parser JSON
parseJNumber = JNumber . read <$> rawNumber

parseJBool :: Parser JSON
parseJBool = (JBool True <$ string "true") <|> (JBool False <$ string "false")

parseJArray :: Parser JSON
parseJArray = parseEmptyArray <|> parseNonEmptyArray

parseEmptyArray :: Parser JSON
parseEmptyArray = do
  _ <- token (char '[')
  _ <- token (char ']')
  return (JArray [])

parseNonEmptyArray :: Parser JSON
parseNonEmptyArray = do
  _ <- token (char '[')
  x <- parseJSON
  xs <- many (token (char ',') *> parseJSON)
  _ <- token (char ']')
  return (JArray (x : xs))


parsePair :: Parser (String, JSON)
parsePair = do
  key <- token escapedString
  _ <- token (char ':')
  value <- parseJSON
  return (key, value)

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJNumber <|> parseJString <|> parseJBool <|> parseJArray
