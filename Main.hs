{-# OPTIONS_GHC -Wall #-}

module Main (matches, main) where

data Regex
   = Nil             -- No string matches this pattern
   | Empty           -- Matches the empty string
   | Symbol Char     -- Matches a specific character
   | Or Regex Regex  -- Matches first or second pattern
   | Seq Regex Regex -- Matches one pattern followed by another
   | Kleene Regex    -- Matches r*
   deriving (Eq, Show)

-- Returns the derivative of the regex with respect to the character
derive :: Regex -> Char -> Regex
derive Nil   _ = Nil
derive Empty _ = Nil
derive (Symbol c) x = if x == c
    then Empty
    else Nil
derive (Or r1 r2) c = Or (derive r1 c) (derive r2 c)
derive (Seq r1 r2) c = if matchesEmpty r1
    then Or (derive r2 c) $ Seq (derive r1 c) r2
    else Seq (derive r1 c) r2
derive (Kleene r1) c = Seq (derive r1 c) (Kleene r1)

-- If the given regex accepts the empty strings, this returns empty.
-- Otherwise, it returns the unmatcheable regex
matchesEmpty :: Regex -> Bool
matchesEmpty Nil = False
matchesEmpty Empty = True
matchesEmpty (Symbol _) = False
matchesEmpty (Or r1 r2) = matchesEmpty r1 || matchesEmpty r2
matchesEmpty (Seq r1 r2) = matchesEmpty r1 && matchesEmpty r2
matchesEmpty (Kleene _) = True

matches :: Regex -> String -> Bool
matches r = matchesEmpty . foldl derive r

main :: IO ()
main = print $ matches regex "aabbc"
  where regex = Seq (Seq (Kleene (Symbol 'a')) (Kleene (Symbol 'b'))) (Kleene (Symbol 'c'))
