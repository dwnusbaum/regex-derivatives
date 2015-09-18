module ParseSpec where

import Test.Hspec
import Test.QuickCheck
import Regex
import Parse

spec :: Spec
spec = do
  describe "Parse.parseRegex" $ do
    it "parses \"a\" as (Symbol a) as long as a is not a reserved symbol" $
      forAll validChar $ \x -> parseRegex [x] `shouldBe` Right (Symbol x)

    it "parses \"a|b\" as (Or (Symbol a) (Symbol b))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex ['(', x, '|', y, ')'] `shouldBe` Right (Or (Symbol x) (Symbol y))

    it "parses \"ab\" as (Seq (Symbol a) (Symbol b))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex [x, y] `shouldBe` Right (Seq (Symbol x) (Symbol y))

    it "parses \"a*\" as (Kleene (Symbol a))" $
      forAll validChar $ \x -> parseRegex [x, '*'] `shouldBe` Right (Kleene (Symbol x))

validChar :: Gen Char
validChar = arbitrary `suchThat` \x -> not $ x `elem` "()|*\\"

main :: IO ()
main = hspec spec
