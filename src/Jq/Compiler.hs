module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Data.Foldable (find)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

compile (StringIndexing key) JNull = return [Jnull]
compile (StringIndexing key) (JObject inp) = 
    case findValueAssociatedToKey key inp of
        Just value -> return [value]
        Nothing -> return [Jnull]
compile (StringIndexing key) _ = Left "The argument was a Json type that is not indexable with the a key"


findValueAssociatedToKey :: Eq a => a -> [(a, b)] -> Maybe b
findValueAssociatedToKey _ [] = Nothing
findValueAssociatedToKey key ((k, v):xs)
    | key == k = Just v
    | otherwise = findValueAssociatedToKey key xs

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
