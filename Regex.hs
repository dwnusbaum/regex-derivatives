-- | This module provides the Regex data type and matching functions
module Regex
    ( Regex(..)
    , Match(..)
    , allMatches
    , matches
    , matchesExact
    ) where

-- | The data type of regexes
data Regex
    = Nil              -- ^ No string matches this pattern
    | Dot              -- ^ Matches any single character
    | Empty            -- ^ Matches the empty string
    | Sym Char         -- ^ Matches a specific character
    | Or  Regex Regex  -- ^ Matches first or second pattern
    | Seq Regex Regex  -- ^ Matches one pattern followed by another
    | Kleene Regex     -- ^ Matches the kleene star of a pattern
    | Group Tag Regex  -- ^ Matches a regex and stores its match in a capturing group
    deriving (Eq, Show)

-- | The data type of matches
data Match = Match
    { start :: Int  -- ^ Start index of the match.
    , size  :: Int  -- ^ The size of the match.
    }
    deriving (Eq, Show)

type Tag = Int

newtype CaptureGroups = CaptureGroups [(Tag, Match)]

-- | Derive a regex with respect to a character.
derive :: Regex -> Char -> Regex
derive Nil   _ = Nil
derive Dot   _ = Empty
derive Empty _ = Nil
derive (Sym c) x
    | x == c = Empty
    | otherwise = Nil
derive (Or  r1 r2) c = Or (derive r1 c) $ derive r2 c
derive (Seq r1 r2) c
    | matchesEmpty r1 = Or (derive r2 c) $ Seq (derive r1 c) r2
    | otherwise = Seq (derive r1 c) r2
derive (Kleene r) c = Seq (derive r c) $ Kleene r

-- | If the given regex accepts the empty string, this returns true.
-- Otherwise it returns false.
matchesEmpty :: Regex -> Bool
matchesEmpty Nil         = False
matchesEmpty Dot         = False
matchesEmpty Empty       = True
matchesEmpty (Sym     _) = False
matchesEmpty (Or  r1 r2) = matchesEmpty r1 || matchesEmpty r2
matchesEmpty (Seq r1 r2) = matchesEmpty r1 && matchesEmpty r2
matchesEmpty (Kleene  _) = True

-- | Match a string to a regex exactly.  Returns True if the full string
-- matches the regex, and False otherwise.
matchesExact :: Regex -> String -> Bool
matchesExact r s
    | matches r s == Just (Match 0 $ length s) = True
    | otherwise = False

-- | Match a string to a regex.  Returns Just the length of the match if the
-- regex matches the string, otherwise Nothing.
matches :: Regex -> String -> Maybe Match
matches r s = matches' r s 0
  where
    matches' r' [] i
        | matchesEmpty r' = Just (Match 0 i)
        | otherwise       = Nothing
    matches' r' (c:cs) i = case matches' (derive r' c) cs $ i + 1 of
        Nothing -> if matchesEmpty r' then Just (Match 0 i) else Nothing
        found   -> found


-- | Returns a list of all matches against a given regex.
-- Matches do not overlap.
allMatches :: Regex -> String -> [Match]
allMatches r s = allMatches' s 0
  where
    allMatches' [] _ = []
    allMatches' cs i = case matches r cs of
        Nothing    -> allMatches' (tail cs) $ i + 1
        Just match -> Match i (size match) : allMatches' (drop (size match) cs) (i + size match)
