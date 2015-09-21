module Main where

import System.Environment

import Parse
import Regex

-- | Pretty prints a single match of a string
showMatch :: String -> Match -> String
showMatch str match = unlines [str, frontPadding (start match) ++ matchString (size match)]
  where
    frontPadding n = replicate n ' '
    matchString n = take n $ '^' : replicate (n - 1) '~'

-- | Given a regex and a string, this function returns a pretty printed list
-- of strings that show where the regex matched.
showMatches :: Regex -> String -> [String]
showMatches r s = map (showMatch s) $ allMatches r s

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> case parseRegex $ head args of
                 Left  err   -> print err
                 Right regex -> putStr $ unlines $ showMatches regex $ args !! 1
        _ -> putStrLn "Wrong number of arguments. Try: regex a* aaaa"

