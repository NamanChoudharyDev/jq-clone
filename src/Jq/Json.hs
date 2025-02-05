module Jq.Json where

data JSON =
     JNull

instance Show JSON where
  show JNull   = "null"
  show _       = undefined

instance Eq JSON where
  JNull == JNull = True
  _ == _ = undefined

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
