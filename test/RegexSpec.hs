{-# Language LambdaCase #-}

module RegexSpec where

import Control.Applicative (liftA, liftA2, pure)
import Test.Hspec
import Test.QuickCheck
import Regex

instance Arbitrary Regex where
  arbitrary = sized arbRegex
    where arbRegex 0 = oneof
            [ pure Empty
            , liftA Symbol arbitrary
            ]
          arbRegex n = frequency
            [ (1, pure Empty)
            , (3, liftA Symbol arbitrary)
            , (2, liftA2 Or subRegex noEmptyRegex)
            , (2, liftA2 Seq subRegex noEmptyRegex)
            , (1, liftA Kleene $ noEmptyRegex `suchThat` \case
                  Kleene _ -> False
                  _ -> True
              )
            ]
            where subRegex = arbRegex (n `div` 2)
                  noEmptyRegex = subRegex `suchThat` \r -> r /= Empty

-- Generates a list of matches for a regex that exhausts all branches of the regex
exhaustiveMatches :: Regex -> [String]
exhaustiveMatches Nil   = []
exhaustiveMatches Empty = [""]
exhaustiveMatches (Symbol x) = [[x]]
exhaustiveMatches (Or r1 r2) = exhaustiveMatches r1 ++ exhaustiveMatches r2
exhaustiveMatches (Seq r1 r2) = concatMap (\m -> map (m ++) $ exhaustiveMatches r2) $ exhaustiveMatches r1
exhaustiveMatches (Kleene r) = "" : rs ++ map (concat . replicate 2) rs
  where rs = exhaustiveMatches r

spec :: Spec
spec =
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
      property $ \r -> all (matches r) (exhaustiveMatches r) `shouldBe` True

main :: IO ()
main = hspec spec
