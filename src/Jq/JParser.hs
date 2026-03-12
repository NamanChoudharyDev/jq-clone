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

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJNumber <|> parseJString <|> parseJBool
