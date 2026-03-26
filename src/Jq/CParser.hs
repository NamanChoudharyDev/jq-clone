module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Parsing.Utils

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- token (char '(')
  p <- parseFilter
  _ <- token (char ')')
  return (Parenthesis p)

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

parseOptionalIdentifierIndexing :: Parser Filter
parseOptionalIdentifierIndexing = do
  _ <- token . char $ '.'
  key <- ident
  _ <- token (char '?')
  return (OptionalObjectIndexing key)

parseGenericIndexing :: Parser Filter
parseGenericIndexing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  key <- token escapedString
  _ <- token (char ']')
  return (StringIndexing key)

parseOptionalGenericIndexing :: Parser Filter
parseOptionalGenericIndexing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  key <- token escapedString
  _ <- token (char ']')
  _ <- token (char '?')
  return (OptionalObjectIndexing key)

parseStringIndexing :: Parser Filter
parseStringIndexing = do
  _ <- token . char $ '.'
  StringIndexing <$> token escapedString

parseOptionalStringIndexing :: Parser Filter
parseOptionalStringIndexing = do
  _ <- token . char $ '.'
  key <- token escapedString
  _ <- token (char '?')
  return (OptionalObjectIndexing key)

parseArrayIndexing :: Parser Filter
parseArrayIndexing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  index <- integer
  _ <- token (char ']')
  return (ArrayIndexing index)

parseArraySlicing :: Parser Filter
parseArraySlicing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  i <- optional integer
  _ <- token (char ':')
  j <- optional integer
  _ <- token (char ']')
  return (ArraySlicing i j)

parseFullIterator :: Parser Filter
parseFullIterator = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  _ <- token (char ']')
  return FullIterator

parseValueIterator :: Parser Filter
parseValueIterator = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  i <- integer
  is <- many (do
    _ <- token (char ',')
    integer)
  _ <- token (char ']')
  return (ValueIterator (i:is))

parseStringValueIterator :: Parser Filter
parseStringValueIterator = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  key <- token escapedString
  keys <- many (do
    _ <- token (char ',')
    token escapedString)
  _ <- token (char ']')
  return (StringValueIterator (key:keys))

parseOptionalArrayIndexing :: Parser Filter
parseOptionalArrayIndexing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  index <- integer
  _ <- token (char ']')
  _ <- token (char '?')
  return (OptionalArrayIndexing index)

parseOptionalArraySlicing :: Parser Filter
parseOptionalArraySlicing = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  i <- optional integer
  _ <- token (char ':')
  j <- optional integer
  _ <- token (char ']')
  _ <- token (char '?')
  return (OptionalArraySlicing i j)

parseOptionalFullIterator :: Parser Filter
parseOptionalFullIterator = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  _ <- token (char ']')
  _ <- token (char '?')
  return OptionalFullIterator

parseOptionalValueIterator :: Parser Filter
parseOptionalValueIterator = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  i <- integer
  is <- many (do
    _ <- token (char ',')
    integer)
  _ <- token (char ']')
  _ <- token (char '?')
  return (OptionalValueIterator (i:is))

parseOptionalStringValueIterator :: Parser Filter
parseOptionalStringValueIterator = do
  _ <- token . char $ '.'
  _ <- token (char '[')
  key <- token escapedString
  keys <- many (do
    _ <- token (char ',')
    token escapedString)
  _ <- token (char ']')
  _ <- token (char '?')
  return (OptionalStringValueIterator (key:keys))

parseFirstChainedFilter :: Parser Filter
parseFirstChainedFilter = parseParenthesis <|> parseOptionalArraySlicing <|> parseArraySlicing <|> parseOptionalFullIterator 
  <|> parseFullIterator <|> parseOptionalArrayIndexing <|> parseArrayIndexing <|> parseOptionalValueIterator 
  <|> parseValueIterator <|> parseOptionalGenericIndexing <|> parseOptionalStringValueIterator <|> parseStringValueIterator
  <|> parseGenericIndexing <|> parseOptionalStringIndexing <|> parseStringIndexing <|> parseOptionalIdentifierIndexing 
  <|> parseIdentifierIndexing

{-
I first had a function filterChaining defined as so:
chainFilters :: Filter -> [Filter] -> Filter
chainFilters f [] = f
chainFilters f (i:is) = chainFilters (Pipe f i) is

and implemented the last line of parseChainedIndexing as the following:
  return (filterChaining firstIndex chainedIndexes)

but then VS code stepped in and said yo just use foldl and then I just removed this helper entirely,
and used foldl inside of parseChainedIndexing
-}
parseChainedIndexing :: Parser Filter
parseChainedIndexing = do
  firstIndex <- parseFirstChainedFilter
  chainedIndexes <- many (parseOptionalArraySlicing <|> parseArraySlicing <|> parseOptionalFullIterator <|> parseFullIterator
    <|> parseOptionalArrayIndexing <|> parseArrayIndexing <|> parseOptionalValueIterator <|> parseValueIterator
    <|> parseOptionalGenericIndexing <|> parseOptionalStringValueIterator <|> parseStringValueIterator <|> parseGenericIndexing
    <|> parseOptionalStringIndexing <|> parseStringIndexing <|> parseOptionalIdentifierIndexing <|> parseIdentifierIndexing)
  return (foldl Pipe firstIndex chainedIndexes)

{-
Again my Hlint gave me the suggestion to turn this code:
  right <- parseFilter -- Used a LLM to get the idea of recursively parsing
  return (Pipe left right)
to this:
  Pipe left <$> parseFilter
-}
parsePipe :: Parser Filter
parsePipe = do
  left <- parseParenthesis <|> parseChainedIndexing <|> parseIdentity
  _ <- token (char '|')
  Pipe left <$> parseFilter

parseComma :: Parser Filter
parseComma = do
  left <- parsePipe <|> parseParenthesis <|> parseChainedIndexing <|> parseIdentity
  _ <- token (char ',')
  Comma left <$> parseFilter

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseComma <|> parseParenthesis <|> parseChainedIndexing <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e