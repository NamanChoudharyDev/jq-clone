module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

compile (StringIndexing _) JNull = return [JNull]
compile (StringIndexing key) (JObject inp) = 
    case findValueAssociatedToKey key inp of
        Just value -> return [value]
        Nothing -> return [JNull]
compile (StringIndexing _) _ = Left "The argument was a Json type that is not indexable with the a key"

compile (Pipe p1 p2) inp = do
    firstPipe <- compile p1 inp
    pipeHelper firstPipe
    where
        pipeHelper [] = return []
        pipeHelper (x:xs) = do
            ys <- compile p2 x
            zs <- pipeHelper xs
            return (ys ++ zs)



findValueAssociatedToKey :: Eq a => a -> [(a, b)] -> Maybe b
findValueAssociatedToKey _ [] = Nothing
findValueAssociatedToKey key ((k, v):xs)
    | key == k = Just v
    | otherwise = findValueAssociatedToKey key xs

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
