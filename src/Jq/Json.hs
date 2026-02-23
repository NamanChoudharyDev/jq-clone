module Jq.Json where

import Text.Printf (printf)
import Data.Char (isControl, ord)

data JSON =
     JNull
  |  JNumber Double
  |  JString String     
  |  JBool Bool
  |  JArray [JSON]
  |  JObject [(String, JSON)] 

instance Show JSON where
  show JNull = "null"
  show (JNumber num) = show num
  show (JString string) = '"': concatMap encodeUnicode string ++ "\""
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JArray array) = "[" ++ formatInput (map show array) ++ "]"
  show (JObject object) = "{" ++ formatInput (map showKeyValuePair object) ++ "}"
    where
      showKeyValuePair (key, value) = show (JString key) ++ ":" ++ show value  

formatInput :: [String] -> String
formatInput [] = ""
formatInput [s] = s
formatInput (s:ss) = s ++ "," ++ formatInput ss

instance Eq JSON where
  JNull == JNull = True
  (JBool a) == (JBool b) = a == b
  (JNumber a) == (JNumber b) = a == b
  (JString a) == (JString b) = a == b
  (JArray a) == (JArray b) = a == b
  (JObject a) == (JObject b) = a == b
  _ == _ = False

encodeUnicode :: Char -> String
encodeUnicode '\b' = "\\b"
encodeUnicode '\f' = "\\f"
encodeUnicode '\n' = "\\n"
encodeUnicode '\r' = "\\r"
encodeUnicode '\t' = "\\t"
encodeUnicode '"' = "\\\""
encodeUnicode '\\' = "\\\\"
encodeUnicode c
  | ord c >= 0x80 && ord c <= 0x9f = [c]
  | isControl c = printf "\\u%04x" (ord c)
  | otherwise = [c]

-- Smart constructors
-- These are included for test purposes and aren't meant to correspond one to one
-- with the actual constructors of the JSON datatype.
-- For the "weekly" tests to succeed fill them in so that they return
-- correct JSON values. Don't change the names or the signatures.

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC = undefined

jsonStringSC :: String -> JSON
jsonStringSC = undefined

jsonBoolSC :: Bool -> JSON
jsonBoolSC = undefined

jsonArraySC :: [JSON] -> JSON
jsonArraySC = undefined

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = undefined
