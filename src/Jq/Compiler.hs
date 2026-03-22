module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]

compile (StringIndexing _) JNull = return [JNull]
compile (StringIndexing key) (JObject inp) =
    case lookup key inp of
        Just value -> return [value]
        Nothing -> return [JNull]
compile (StringIndexing _) _ = Left "The argument was a JSON type that is not indexable with a key"

compile (OptionalObjectIndexing _) JNull = return [JNull]
compile (OptionalObjectIndexing key) (JObject inp) =
    case lookup key inp of
        Just value -> return [value]
        Nothing -> return [JNull]
compile (OptionalObjectIndexing _) _ = return []

compile (Pipe p1 p2) inp = do
    firstPipeFilter <- compile p1 inp
    pipeHelper firstPipeFilter
    where
        pipeHelper [] = return []
        pipeHelper (x:xs) = do
            ys <- compile p2 x
            zs <- pipeHelper xs
            return (ys ++ zs)

compile (Comma c1 c2) inp = do
    firstCommaFilter <- compile c1 inp
    secondCommaFilter <- compile c2 inp
    return (firstCommaFilter ++ secondCommaFilter)

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p = p -- VS code recommended that this can be eta reduced by removing the j parameter
