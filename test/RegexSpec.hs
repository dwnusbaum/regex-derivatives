module RegexSpec where

import Test.Hspec
import Test.QuickCheck
import Regex

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

    it "matches (Kleene (Or (Symbol a) (Symbol b))) against a or b zero or more times" $
      property $ \x y i ->
        let regex = Kleene (Or (Symbol x) (Symbol y))
        in matches regex ""
        && matches regex (replicate i x)
        && matches regex (replicate i y)
        && matches regex [x,y,x,y,y,x,x] `shouldBe` True

main :: IO ()
main = hspec spec
