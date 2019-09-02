module Main where

import           System.Utils (argOr, putStderr)

main :: IO ()
main = do
  input <- argOr 0 "statements.txt"
  output <- argOr 1 "data.json"
  putStderr $ "Processing statements from: \"" ++ input ++ "\""
  putStderr $ "Data written to: \"" ++ output ++ "\""
