module Jq.Json where

import Text.Printf (printf)
import Data.Char (isControl, ord)
import Data.List (sortOn)

data JSON =
     JNull
  |  JNumber Double
  |  JString String     
  |  JBool Bool
  |  JArray [JSON]
  |  JObject [(String, JSON)] 

instance Show JSON where
  show = showHelper 0  -- Visual Studio Code hlint gave me the eta reduce suggestion (the original line was: show input = showHelper 0 input)

showHelper :: Int -> JSON -> String
showHelper _ JNull = "null"
showHelper _ (JNumber num)
  | num == fromInteger (round num) = show (round num :: Integer)
  | otherwise = show num
showHelper _ (JString string) = '"': concatMap encodeUnicode string ++ "\""
showHelper _ (JBool True) = "true"
showHelper _ (JBool False) = "false"
showHelper _ (JArray []) = "[]"
showHelper num (JArray array) = "[" ++ "\n" ++ formatInput (num + 2) (map (showHelper (num + 2)) array) ++ "\n" ++ indent num ++ "]"
showHelper _ (JObject []) = "{}"
showHelper num (JObject object) = "{" ++ "\n" ++ formatInput (num + 2) (map showKeyValuePair (sortOn fst object)) ++ "\n" ++ indent num ++ "}"
  where
    showKeyValuePair (key, value) = showHelper (num + 2) (JString key) ++ ": " ++ showHelper (num + 2) value

formatInput :: Int -> [String] -> String
formatInput _ [] = ""
formatInput num [s] = indent num ++ s
formatInput num (s:ss) = indent num ++ s ++ "," ++ "\n" ++ formatInput num ss

indent :: Int -> String
indent num = replicate num ' '

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
jsonNumberSC = JNumber . fromIntegral

jsonStringSC :: String -> JSON
jsonStringSC = JString

jsonBoolSC :: Bool -> JSON
jsonBoolSC = JBool

jsonArraySC :: [JSON] -> JSON
jsonArraySC = JArray

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC = JObject
