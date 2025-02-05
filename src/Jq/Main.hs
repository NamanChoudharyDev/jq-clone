module Jq.Main (main, process) where

import Jq.Filters (filters)
import Jq.Parser
import Jq.Compiler

import Control.Arrow (left)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

getInputs :: IO ([String], String)
getInputs = do
   -- all program's command line arguments
  args <- getArgs
   -- all stdin input
  toparse <- getContents
  return (args, toparse)

-- We use this function for testing, please don't change its signature
process :: [String] -> String -> Either String String
process args json = do
  v <- parseConfig args
  -- convert Maybe to Either, adding an error message
  obj <- maybe (Left "Couldn't parse JSON") Right $ parse parseJSON json
  -- you can modify the type of `compile` to output a compilation error too
  let program = compile . filters $ v
  -- prepend the execution error with an explanation
  res <- left ("Couldn't execute the program: " ++) $ run program obj
  -- convert all JSON output objects to String
  -- and add a newline at the end of each printed object
  return $ concatMap ((++"\n") . show) res

main :: IO ()
main = do
  (args, inp) <- getInputs
  case process args inp of
    Left e -> putErr e
    Right v -> putStr v
  where
    putErr :: String -> IO ()
    putErr = hPutStrLn stderr
