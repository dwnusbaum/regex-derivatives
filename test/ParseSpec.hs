module ParseSpec where

import Test.Hspec
import Test.QuickCheck
import Regex
import Parse

spec :: Spec
spec = do
  describe "Parse.parseRegex" $ do
    it "parses \"a\" as (Symbol a) if a is not a reserved symbol" $
      forAll validChar $ \x -> parseRegex [x] `shouldBe` Right (Symbol x)

    it "parses \"\\a\" as (Symbol a) if a is a reserved symbol" $
      forAll invalidChar $ \x -> parseRegex ['\\', x] `shouldBe` Right (Symbol x)

    it "parses \"a|b\" as (Or (Symbol a) (Symbol b))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex ['(', x, '|', y, ')'] `shouldBe` Right (Or (Symbol x) (Symbol y))

    it "parses \"[abc]\" as (Or (Symbol a) (Or (Symbol b) (Symbol c)))" $
      forAll (vectorOf 3 validChar) $ \xs -> parseRegex ('[' : xs ++ "]") `shouldBe` Right (Or (Symbol $ head xs) (Or (Symbol $ xs !! 1) (Symbol $ xs !! 2)))

    it "parses \"ab\" as (Seq (Symbol a) (Symbol b))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex [x, y] `shouldBe` Right (Seq (Symbol x) (Symbol y))

    it "parses \"a*\" as (Kleene (Symbol a))" $
      forAll validChar $ \x -> parseRegex [x, '*'] `shouldBe` Right (Kleene (Symbol x))

    it "parses \"ab*\" as (Seq (Symbol a) (Kleene (Symbol a)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex [x, y, '*'] `shouldBe` Right (Seq (Symbol x) (Kleene (Symbol y)))

validChar :: Gen Char
validChar = arbitrary `suchThat` \x -> not $ x `elem` reserved

invalidChar :: Gen Char
invalidChar = arbitrary `suchThat` \x -> x `elem` reserved

main :: IO ()
main = hspec spec
