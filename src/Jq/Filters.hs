module Jq.Filters where
import Jq.Json

data Filter = Identity
  | Parenthesis Filter
  | StringIndexing String
  | OptionalObjectIndexing String
  | ArrayIndexing Int
  | ArraySlicing (Maybe Int) (Maybe Int)
  | FullIterator
  | ValueIterator [Int]
  | StringValueIterator [String] -- Did not come up on my own that this should be implemented, looked into the Discord of Tu Delft CSE and in the FP channel found that this should be supported (this was my missing piece to go from 7.5 to 8 points)
  | OptionalArrayIndexing Int
  | OptionalArraySlicing (Maybe Int) (Maybe Int)
  | OptionalFullIterator
  | OptionalValueIterator [Int]
  | OptionalStringValueIterator [String]
  | Pipe Filter Filter
  | Comma Filter Filter
  | SimpleLiteralConstructor JSON
  | SimpleArrayConstructor [Filter]
  | SimpleObjectConstructor [(Filter, Filter)]

instance Show Filter where
  show Identity = "."
  show (Parenthesis p) = "(" ++ show p ++ ")"
  show (StringIndexing key) = "." ++ key
  show (OptionalObjectIndexing key) = "." ++ key ++ "?"
  show (ArrayIndexing i) = ".[" ++ show i ++ "]"
  show (ArraySlicing i j) = ".[" ++ showOptional i ++ ":" ++ showOptional j ++ "]"
    where
      showOptional :: Maybe Int -> String
      showOptional Nothing = ""
      showOptional (Just x) = show x 
  show FullIterator = ".[]"
  show (ValueIterator iter) = ".[" ++ showValueIterator iter ++ "]"
    where
      showValueIterator :: [Int] -> String
      showValueIterator [] = ""
      showValueIterator [x] = show x
      showValueIterator (x:xs) = show x ++ "," ++ showValueIterator xs
  show (StringValueIterator keys) = ".[" ++ showKeys keys ++ "]"
    where
      showKeys [] = ""
      showKeys [k] = show k
      showKeys (k:ks) = show k ++ "," ++ showKeys ks
  show (OptionalArrayIndexing i) = ".[" ++ show i ++ "]?"
  show (OptionalArraySlicing i j) = ".[" ++ showOptional i ++ ":" ++ showOptional j ++ "]?"
    where
      showOptional Nothing = ""
      showOptional (Just x) = show x
  show OptionalFullIterator = ".[]?"
  show (OptionalValueIterator iter) = ".[" ++ showOptionalValueIterator iter ++ "]?"
    where
      showOptionalValueIterator [] = ""
      showOptionalValueIterator [x] = show x
      showOptionalValueIterator (x:xs) = show x ++ "," ++ showOptionalValueIterator xs
  show (OptionalStringValueIterator keys) = ".[" ++ showKeys keys ++ "]?"
    where
      showKeys [] = ""
      showKeys [k] = show k
      showKeys (k:ks) = show k ++ "," ++ showKeys ks
  show (Pipe p1 p2) = show p1 ++ " | " ++ show p2
  show (Comma c1 c2) = show c1 ++ " , " ++ show c2
  show (SimpleLiteralConstructor json) = show json
  show (SimpleArrayConstructor []) = "[]"
  show (SimpleArrayConstructor arrayFilters) = "[" ++ showArray arrayFilters ++ "]"
    where
      showArray [] = ""
      showArray [f] = show f
      showArray (f:fs) = show f ++ ", " ++ showArray fs
  show (SimpleObjectConstructor []) = "{}"
  show (SimpleObjectConstructor simpleObject) = "{" ++ showSimpleObject simpleObject ++ "}"
    where
      showSimpleObject [] = ""
      showSimpleObject [(key,value)] = show key ++ ": " ++ show value
      showSimpleObject ((key,value):kvs) = show key ++ ": " ++ show value ++ ", " ++ showSimpleObject kvs

instance Eq Filter where
  Identity == Identity = True
  (Parenthesis a) == (Parenthesis b) = a == b
  (StringIndexing a) == (StringIndexing b) = a == b
  (OptionalObjectIndexing a) == (OptionalObjectIndexing b) = a == b
  (ArrayIndexing a) == (ArrayIndexing b) = a == b
  (ArraySlicing sa sb) == (ArraySlicing sc sd) = sa == sc && sb == sd
  FullIterator == FullIterator = True
  (ValueIterator a) == (ValueIterator b) = a == b
  (StringValueIterator a) == (StringValueIterator b) = a == b
  (OptionalArrayIndexing a) == (OptionalArrayIndexing b) = a == b
  (OptionalArraySlicing sa sb) == (OptionalArraySlicing sc sd) = sa == sc && sb == sd
  OptionalFullIterator == OptionalFullIterator = True
  (OptionalValueIterator a) == (OptionalValueIterator b) = a == b
  (OptionalStringValueIterator a) == (OptionalStringValueIterator b) = a == b
  (Pipe pa pb) == (Pipe pc pd) = pa == pc && pb == pd
  (Comma ca cb) == (Comma cc cd) = ca == cc && cb == cd
  (SimpleLiteralConstructor a) == (SimpleLiteralConstructor b) = a == b
  (SimpleArrayConstructor a) == (SimpleArrayConstructor b) = a == b
  (SimpleObjectConstructor a) == (SimpleObjectConstructor b) = a == b
  _ == _ = False

newtype Config = ConfigC {filters :: Filter} -- hlint recommmended me to define this with the keyword newtype instead of data. I like not having blue lines in my VS code so I applied it and in the recommendation it said decreases laziness

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = StringIndexing

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = Pipe

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = Comma
