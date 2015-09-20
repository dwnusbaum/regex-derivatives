module Main where

import System.Environment

import Parse
import Regex

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> case parseRegex $ head args of
               Left  err -> print err
               Right reg -> print $ matches reg $ head $ tail args
        _ -> putStrLn "Wrong number of arguments. Try: regex a* aaaa"

