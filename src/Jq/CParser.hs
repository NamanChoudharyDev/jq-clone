module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Parsing.Utils

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

{-
  For this parser my VS code said my code:
    key <- ident
    return (StringIndexing key)
  can be simplified to:
  StringIndexing <$> ident
  I applied this
-}
parseIdentifierIndexing :: Parser Filter
parseIdentifierIndexing = do
  _ <- token . char $ '.'
  StringIndexing <$> ident

parseGenericIndexing :: Parser Filter
parseGenericIndexing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  key <- token escapedString
  _ <- token (char ']')
  return (StringIndexing key)

{-
Again my Hlint gave me the suggestion to turn this code:
  right <- parseFilter
  return (Pipe left right)
to this:
  Pipe left <$> parseFilter
-}
parsePipe :: Parser Filter
parsePipe = do
  left <- parseGenericIndexing <|> parseIdentifierIndexing <|> parseIdentity
  _ <- token (char '|')
  Pipe left <$> parseFilter

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseGenericIndexing <|> parseIdentifierIndexing <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
