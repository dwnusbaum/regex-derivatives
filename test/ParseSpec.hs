module ParseSpec where

import Test.Hspec
import Test.QuickCheck

import Parse
import Regex

validChar :: Gen Char
validChar = arbitrary `suchThat` (`notElem` reserved)

invalidChar :: Gen Char
invalidChar = arbitrary `suchThat` (`elem` reserved)

spec :: Spec
spec =
  describe "Parse.parseRegex" $ do
    it "parses \".\" as Dot" $
      parseRegex "." `shouldBe` Right Dot

    it "parses \"a\" as (Symbol a) if a is not a reserved symbol" $
      forAll validChar $ \x -> parseRegex [x] `shouldBe` Right (Symbol x)

    it "parses \"\\a\" as (Symbol a) if a is a reserved symbol" $
      forAll invalidChar $ \x -> parseRegex ['\\', x] `shouldBe` Right (Symbol x)

    it "parses \"a|b|c\" as (Or (Symbol a) (Or (Symbol b) (Symbol c)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y ->
      forAll validChar $ \z -> parseRegex ['(', x, '|', y, '|', z, ')'] `shouldBe` Right (Or (Or (Symbol x) (Symbol y)) (Symbol z))

    it "parses \"[abc]\" as (Or (Symbol a) (Or (Symbol b) (Symbol c)))" $
      forAll (vectorOf 3 validChar) $ \xs -> parseRegex ('[' : xs ++ "]") `shouldBe` Right (Or (Symbol $ head xs) (Or (Symbol $ xs !! 1) (Symbol $ xs !! 2)))

    it "parses \"abc\" as (Seq (Symbol a) (Seq (Symbol b) (Symbol c)))" $
      forAll (vectorOf 3 validChar) $ \xs -> parseRegex xs `shouldBe` Right (Seq (Seq (Symbol $ head xs) (Symbol $ xs !! 1)) (Symbol $ xs !! 2))

    it "parses \"a*\" as (Kleene (Symbol a))" $
      forAll validChar $ \x -> parseRegex [x, '*'] `shouldBe` Right (Kleene (Symbol x))

    it "parses \"a?\" as (Or Empty (Symbol a))" $
      forAll validChar $ \x -> parseRegex [x, '?'] `shouldBe` Right (Or Empty (Symbol x))

    it "parses \"ab*\" as (Seq (Symbol a) (Kleene (Symbol a)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex [x, y, '*'] `shouldBe` Right (Seq (Symbol x) (Kleene (Symbol y)))

    it "parses \"[ab]*\" as (Kleene (Or (Symbol a) (Symbol b)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y -> parseRegex ['[', x, y, ']', '*'] `shouldBe` Right (Kleene (Or (Symbol x) (Symbol y)))

    it "parses \"([ab]c|.d)*\" as (Kleene (Or (Seq (Or (Symbol a) (Symbol b)) (Symbol y)) (Seq Dot (Symbol d)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y ->
      forAll validChar $ \w ->
      forAll validChar $ \z -> parseRegex ['(', '[', x, y, ']', w, '|', '.', z, ')', '*'] `shouldBe` Right (Kleene (Or (Seq (Or (Symbol x) (Symbol y)) (Symbol w)) (Seq Dot (Symbol z))))

main :: IO ()
main = hspec spec
