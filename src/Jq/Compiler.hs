module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile Identity inp = return [inp]

compile (Parenthesis f) inp = compile f inp

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

compile (ArrayIndexing _) JNull = return [JNull]
compile (ArrayIndexing i) (JArray xs)
    | index >= 0 && index < n = return [xs !! index]
    | otherwise = return [JNull]
    where
        n = length xs
        index = if i < 0 then n + i else i
compile (ArrayIndexing _) _ =
    Left "The argument was a JSON type that is not indexable with an array index"

compile (ArraySlicing _ _) JNull = return [JNull]
compile (ArraySlicing i j) (JArray xs) = return [JArray (sliceArray i j xs)]
compile (ArraySlicing _ _) _ =
    Left "The argument was a JSON type that is not sliceable"

compile FullIterator (JArray xs) = return xs
compile FullIterator (JObject inp) = return (map snd inp)
compile FullIterator _ = Left "The argument was a JSON type that is not iterable"

compile (ValueIterator iter) (JArray xs) = return (arrayIteratorHelper iter xs)
compile (ValueIterator _) _ = Left "The argument was a JSON type that is not iterable with the value iterator"

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

-----------------------------------------------------------------------------------------------------------------------------------------------------------
sliceArray :: Maybe Int -> Maybe Int -> [JSON] -> [JSON]
sliceArray i j xs
    | putStartInBounds >= putEndInBounds = []
    | otherwise = take (putEndInBounds - putStartInBounds) (drop putStartInBounds xs)
  where
    n = length xs

    start = case i of
        Nothing -> 0
        Just k -> if k < 0 then n + k else k

    end = case j of
        Nothing -> n
        Just k -> if k < 0 then n + k else k
    -- Used a LLM to generate test cases to debug to find this fix for putting the indices always in bounds (the LLM did not write the code it just provided me test inputs that should pass in jq)
    putStartInBounds = max 0 (min n start)
    putEndInBounds = max 0 (min n end)

arrayIteratorHelper :: [Int] -> [JSON] -> [JSON]
arrayIteratorHelper [] _ = []
arrayIteratorHelper (i:is) xs = arrayIndexHelper i xs : arrayIteratorHelper is xs

arrayIndexHelper :: Int -> [JSON] -> JSON
arrayIndexHelper i xs
    | index >= 0 && index < n = xs !! index
    | otherwise = JNull
  where
    n = length xs
    index = if i < 0 then n + i else i

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p = p -- VS code recommended that this can be eta reduced by removing the j parameter
