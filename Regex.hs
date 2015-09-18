-- | This module provides the Regex data type and matching functions
module Regex
  ( Regex(..)
  , matches
  ) where

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
derive (Symbol c) x
  | x == c = Empty
  | otherwise = Nil
derive (Or r1 r2) c = Or (derive r1 c) (derive r2 c)
derive (Seq r1 r2) c
  | matchesEmpty r1 = Or (derive r2 c) $ Seq (derive r1 c) r2
  | otherwise = Seq (derive r1 c) r2
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

-- Returns true if the regex matches the string
matches :: Regex -> String -> Bool
matches r = matchesEmpty . foldl derive r
