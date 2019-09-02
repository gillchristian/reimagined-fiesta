module System.Utils
  ( nthArg
  , argOr
  , putStderr
  ) where

import qualified Control.Applicative as Ap
import qualified Data.Maybe          as M
import qualified System.Environment  as Env
import qualified System.IO           as Sys

nth :: [a] -> Int -> Maybe a
nth xs n
  | n >= length xs = Nothing
  | n < 0 = Nothing
  | otherwise = Just (xs !! n)

nthArg :: Int -> IO (Maybe String)
nthArg = Ap.liftA2 nth Env.getArgs . pure

argOr :: Int -> String -> IO String
argOr n def = M.fromMaybe def <$> nthArg n

putStderr :: String -> IO ()
putStderr = Sys.hPutStrLn Sys.stderr
