module Parsing.Utils where

import Parsing.Parsing
import Data.Char (isDigit, isControl, digitToInt, chr, isHexDigit)

-- Skip over, useful for conditional parsing
skip :: Parser String
skip = return ""

nonZeroDigit :: Char -> Bool
nonZeroDigit d = isDigit d && (d /= '0')

regularNumber :: Parser String
regularNumber = do
    start <- sat nonZeroDigit
    end <- many digit
    return (start:end)

-- This overwrites <&> from Data.Functor
(<&>) :: Parser String -> Parser String -> Parser String
p1 <&> p2 = liftA2 (++) p1 p2

satString :: (Char -> Bool) -> Parser String
satString predicate = (:[]) <$> sat predicate

-- Parse raw number, but return as a string to map into appropriate type
rawNumber :: Parser String
rawNumber = (string "-" <|> skip         )                     <&>
            (string "0"   <|> regularNumber)                     <&>
            ((string "."  <&> some digit   ) <|> skip)           <&>
            (((string "E" <|> string "e")                        <&>
            (string "-"   <|> string "+" <|> skip) <&> some digit) <|> skip)

hexStringToInt' :: Int -> String -> Int
hexStringToInt' = foldl (\ n x -> 16 * n + digitToInt x) -- Applied suggestion of VS code that it can be written with foldl and applied eta reduce

hexStringToInt :: String -> Int
hexStringToInt = hexStringToInt' 0 -- Applied eta reduce to this line, to get rid of the VS code blue line suggestion

escapeSequence :: Parser Char
escapeSequence = char '"'            <|>
                 char '\\'           <|>
                 char '/'            <|>
                 ('\b' <$ char 'b')  <|>
                 ('\f' <$ char 'f')  <|>
                 ('\n' <$ char 'n')  <|>
                 ('\r' <$ char 'r')  <|>
                 ('\t' <$ char 't')  <|>
                 char 'u' *> (chr . hexStringToInt <$> (satString isHexDigit <&> satString isHexDigit <&> satString isHexDigit <&> satString isHexDigit))

stringAtom :: Parser Char
stringAtom = sat regularStringChar <|> (char '\\' *> escapeSequence)
    where
        regularStringChar p
            | p == '"'     = False
            | p == '\\'    = False
            | isControl p = False
            | otherwise    = True

escapedString :: Parser String
escapedString = char '"' *> many stringAtom <* char '"'