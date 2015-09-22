-- | This module provides the main executable for matching regexes from the
-- command line.  It also provides a function for getting a pretty printed
-- list of matches.
module Main
    ( main
    , showAllMatches
    ) where

import qualified System.Console.ANSI as ANSI
import           System.Environment  (getArgs)

import Parse
import Regex

-- | This string, if output to a UNIX terminal, will reset the terminal to its default display
resetSGRCode :: String
resetSGRCode = ANSI.setSGRCode [ANSI.Reset]

-- | Returns a two line string, where the first line contains the original text and
-- the second line underlines the matching parts
showMatchesLine :: String -> Int -> [Match] -> String
showMatchesLine _ _ [] = ""
showMatchesLine s i ms = prefix ++ highlightMatches s ms
  where
    prefix = ANSI.setSGRCode prefixCode ++ show i ++ resetSGRCode ++ ":"
    prefixCode = [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow
                 , ANSI.SetConsoleIntensity ANSI.BoldIntensity
                 ]

-- | Highlights all of the matches on a particular line using ANSI SGR codes
highlightMatches :: String -> [Match] -> String
highlightMatches s = go s . relative 0
  where
    go str []     = str
    go str (m:ms) = before ++ matchSGRCode ++ match ++ resetSGRCode ++ highlightMatches after ms
      where
        (before, matchAndAfter) = splitAt (start m) str
        (match, after) = splitAt (size m) matchAndAfter
        matchSGRCode = ANSI.setSGRCode [ ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Green
                                       , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black
                                       ]
    relative _ []     = []
    relative i (m:ms) = m { start = start m - i } : relative (start m - i + size m) ms

-- | Returns a string with a carat (^) in the location of each match start and
--  tildes (~) underlining the rest of the match.  This function assumes that
-- the string is will be displayed under also starts at index 0 on the display
-- i.e. underlineMatches [Match 0 2, Match 3 3] would return "^~ ^~~".
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
showAllMatches r s = filter (not . null) $ map (\(s', i) -> showMatchesLine s' i $ allMatches r s') $ zip (lines s) [1..]

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> case parseRegex $ head args of
                 Left  err   -> print err
                 Right regex -> putStr $ unlines $ showAllMatches regex $ args !! 1
        _ -> putStrLn "Wrong number of arguments. Try: regex a* aaaa"

