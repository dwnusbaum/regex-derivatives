module Main where

import System.Environment

import Parse
import Regex

-- | Returns a two line string, where the first line is the original text and
-- the second line underlines the matching parts
showMatchesLine :: String -> [Match] -> String
showMatchesLine _ [] = ""
showMatchesLine s ms = unlines [s, underlineMatches ms]

-- | Returns a string with a carat (^) in the location of each match start and
--  tildes (~) underlining the rest of the match.
-- i.e. showMatchesLine [Match 0 2, Match 3 3] would return "^~ ^~~"
underlineMatches :: [Match] -> String
underlineMatches ms = fst $ foldl underline ("", 0) ms
  where
    underline (str, strLen) (Match mStart mSize) = (str ++ padMatch ++ displayMatch, strLen')
      where
        strLen' = mStart - strLen + mSize
        padMatch = replicate (mStart - strLen) ' '
        displayMatch = take mSize $ '^' : replicate (mSize - 1) '~'

-- | Given a regex and a string, this function returns a pretty printed list
-- of strings that show where the regex matched the string.
showAllMatches :: Regex -> String -> [String]
showAllMatches r s = filter (not . null) $ map (\s' -> showMatchesLine s' $ allMatches r s') $ lines s

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> case parseRegex $ head args of
                 Left  err   -> print err
                 Right regex -> putStr $ concat $ showAllMatches regex $ args !! 1
        _ -> putStrLn "Wrong number of arguments. Try: regex a* aaaa"

