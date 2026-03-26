module Jq.Filters where

data Filter = Identity
  | Parenthesis Filter
  | StringIndexing String
  | OptionalObjectIndexing String
  | ArrayIndexing Int
  | ArraySlicing (Maybe Int) (Maybe Int)
  | FullIterator
  | ValueIterator [Int]
  | Pipe Filter Filter
  | Comma Filter Filter

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
  show (Pipe p1 p2) = show p1 ++ " | " ++ show p2
  show (Comma c1 c2) = show c1 ++ " , " ++ show c2

instance Eq Filter where
  Identity == Identity = True
  (Parenthesis a) == (Parenthesis b) = a == b
  (StringIndexing a) == (StringIndexing b) = a == b
  (OptionalObjectIndexing a) == (OptionalObjectIndexing b) = a == b
  (ArrayIndexing a) == (ArrayIndexing b) = a == b
  (ArraySlicing sa sb) == (ArraySlicing sc sd) = sa == sc && sb == sd
  FullIterator == FullIterator = True
  (ValueIterator a) == (ValueIterator b) = a == b
  (Pipe pa pb) == (Pipe pc pd) = pa == pc && pb == pd
  (Comma ca cb) == (Comma cc cd) = ca == cc && cb == cd
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
