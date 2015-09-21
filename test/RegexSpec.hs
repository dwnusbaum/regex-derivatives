{-# Language LambdaCase #-}

module RegexSpec where

import Control.Applicative (liftA, liftA2, pure)
import Test.Hspec
import Test.QuickCheck

import Regex

instance Arbitrary Regex where
  arbitrary = sized arbRegex
    where
      arbRegex 0 = oneof
        [ pure Empty
        , pure Dot
        , liftA Sym arbitrary
        ]
      arbRegex n = frequency
        [ (1, pure Empty)
        , (1, pure Dot)
        , (3, liftA Sym arbitrary)
        , (2, liftA2 Or subRegex noEmptyRegex)
        , (2, liftA2 Seq subRegex noEmptyRegex)
        , (1, liftA Kleene $ noEmptyRegex `suchThat` \case
              Kleene _ -> False
              _        -> True
          )
        ]
        where
          subRegex     = arbRegex (n `div` 2)
          noEmptyRegex = subRegex `suchThat` \r -> r /= Empty

-- | Generates a list of matches for a regex that exhausts all branches of the
-- regex.
exhaustiveMatches :: Regex -> [String]
exhaustiveMatches Nil         = []
exhaustiveMatches Dot         = ["."] -- Really could be any character
exhaustiveMatches Empty       = [""]
exhaustiveMatches (Sym     c) = [[c]]
exhaustiveMatches (Or  r1 r2) = exhaustiveMatches r1 ++ exhaustiveMatches r2
exhaustiveMatches (Seq r1 r2) = concatMap (\m -> map (m ++) $ exhaustiveMatches r2) $ exhaustiveMatches r1
exhaustiveMatches (Kleene  r) = "" : rs ++ map (concat . replicate 2) rs
  where
    rs = exhaustiveMatches r

spec :: Spec
spec =
  describe "Regex" $ do
    describe "matches" $ do
      it "never matches if the regex is Nil" $
        property $ \x -> matchesExact Nil x `shouldBe` False

      it "matches Dot against any character c" $
        property $ \x -> matchesExact Dot [x] `shouldBe` True

      it "matches Empty against the empty string and nothing else" $
        property $ \x ->
          case x of
            [] -> matchesExact Empty x `shouldBe` True
            _  -> matchesExact Empty x `shouldBe` False

      it "matches (Sym c) against the single character c" $
        property $ \x -> matchesExact (Sym x) [x] `shouldBe` True

      it "matches (Or a b) against either a or b" $
        property $ \x y ->
          let regex = Or (Sym x) (Sym y)
          in matchesExact regex [x] && matchesExact regex [y] `shouldBe` True

      it "matches (Seq a b) against a followed by b" $
        property $ \x y ->
          let regex = Seq (Sym x) (Sym y)
          in matchesExact regex [x, y] `shouldBe` True

      it "matches (Kleene (Sym a)) against a zero or more times" $
        property $ \x i ->
          let regex = Kleene (Sym x)
          in matchesExact regex "" && matchesExact regex (replicate i x) `shouldBe` True

      it "matches arbitrary regexes against all of their minimal matches" $
        property $ \r -> all (matchesExact r) (exhaustiveMatches r) `shouldBe` True

      it "matches a pathological string in polynomial time" $
        property $ \(Positive n) ->
          let r1 = foldl1 Seq $ replicate n (Or Empty (Sym 'x'))
              r2 = foldl1 Seq $ replicate n (Sym 'x')
          in matchesExact (Seq r1 r2) (replicate n 'x') `shouldBe` True

    describe "allMatches" $
      it "returns all non-overlapping matches of a regex in a string" $
        property $ \x ->
        forAll (arbitrary `suchThat` (x /=)) $ \y -> allMatches (Sym x) [x,y,x,y,x,y] `shouldBe` [Match 0 1, Match 2 1, Match 4 1]

main :: IO ()
main = hspec spec
