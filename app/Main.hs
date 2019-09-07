module Main where

import Text.Parsec.Combinator as Comb
import Text.Parsec.String
import Text.Gym.Parse as Gym

main :: IO ()
main = do
  result <- parseFromFile Gym.fileParser "test.txt"
  case result of
    Right xs -> print xs
    Left err -> print err
  case result of
    Right xs -> putStrLn $ "Count: " <> show (length xs)
    Left err -> putStrLn "Count: --"
