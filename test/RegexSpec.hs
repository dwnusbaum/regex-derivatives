module RegexSpec where

import Control.Applicative (liftA, liftA2, pure)
import Test.Hspec
import Test.QuickCheck
import Regex

instance Arbitrary Regex where
  arbitrary = sized arbRegex
    where arbRegex 0 = oneof [pure Empty, liftA Symbol arbitrary]
          arbRegex n = oneof
            [ liftA Symbol arbitrary
            , liftA2 Or (oneof [pure Empty, subRegex]) subRegex
            , liftA2 Seq subRegex subRegex
            , liftA Kleene subRegex ]
            where subRegex = arbRegex $ n `div` 2

-- Generates a list of all minimal matches for a regex
minimalMatches :: Regex -> [String]
minimalMatches Nil   = []
minimalMatches Empty = [""]
minimalMatches (Symbol x) = [[x]]
minimalMatches (Or r1 r2) = minimalMatches r1 ++ minimalMatches r2
minimalMatches (Seq r1 r2) = concatMap (\m -> map (m ++) r2s) $ minimalMatches r1
  where r2s = minimalMatches r2
minimalMatches (Kleene r) = "" : matches ++ map (concat . replicate 2) matches
  where matches = minimalMatches r

spec :: Spec
spec = do
  describe "Regex.matches" $ do
    it "never matches if the regex is Nil" $
      property $ \x -> matches Nil x `shouldBe` False

    it "matches Empty against the empty string and nothing else" $
      property $ \x ->
        case x of
          [] -> matches Empty x `shouldBe` True
          _  -> matches Empty x `shouldBe` False

    it "matches (Symbol c) against the single character c" $
      property $ \x -> matches (Symbol x) [x] `shouldBe` True

    it "matches (Or a b) against either a or b" $
      property $ \x y ->
        let regex = Or (Symbol x) (Symbol y)
        in matches regex [x] && matches regex [y] `shouldBe` True

    it "matches (Seq a b) against a followed by b" $
      property $ \x y ->
        let regex = Seq (Symbol x) (Symbol y)
        in matches regex [x, y] `shouldBe` True

    it "matches (Kleene (Symbol a)) against a zero or more times" $
      property $ \x i ->
        let regex = Kleene (Symbol x)
        in matches regex "" && matches regex (replicate i x) `shouldBe` True

    it "matches arbitrary regexes against all of their minimal matches" $
      property $ \r -> all (matches r) (minimalMatches r) `shouldBe` True

main :: IO ()
main = hspec spec
