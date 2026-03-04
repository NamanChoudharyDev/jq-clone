module Jq.Filters where

data Filter = Identity
  | StringIndexing String
  | Pipe Filter Filter
  | Comma Filter Filter

instance Show Filter where
  show Identity = "."
  show (StringIndexing key) = "." ++ show key
  show (Pipe p1 p2) = show p1 ++ " | " ++ show p2
  show (Comma c1 c2) = show c1 ++ " , " ++ show c2

instance Eq Filter where
  Identity == Identity = True
  _ == _ = undefined

newtype Config = ConfigC {filters :: Filter} -- hlint recommmended me to define this with the keyword newtype instead of data. I like not having blue lines in my VS code so I applied it and in the recommendation it said decreases laziness

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC = undefined

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC = undefined

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC = undefined
