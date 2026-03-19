module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

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


parseFilter :: Parser Filter
parseFilter = parseIdentifierIndexing <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e

-- >>> parse parseFilter ".foo"
-- [(.foo,"")]

-- >>> parse parseFilter ".foo_bar"
-- [(.foo_bar,"")]

-- >>>  parse parseFilter "."
-- [(.,"")]
