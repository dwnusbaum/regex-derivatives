module ParseSpec where

import Test.Hspec
import Test.QuickCheck

import Parse
import Regex

-- | Generates characters that are not special to the parser
validChar :: Gen Char
validChar = arbitrary `suchThat` (`notElem` reserved)

-- | Generates characters that are special to the parser
invalidChar :: Gen Char
invalidChar = arbitrary `suchThat` (`elem` reserved)

spec :: Spec
spec =
  describe "Parse.parseRegex" $ do
    it "parses \".\" as Dot" $
      parseRegex "." `shouldBe` Right Dot

    it "parses \"a\" as (Sym a) if a is not a reserved symbol" $
      forAll validChar $ \x -> parseRegex [x] `shouldBe` Right (Sym x)

    it "parses \"\\a\" as (Sym a) if a is a reserved symbol" $
      forAll invalidChar $ \x -> parseRegex ['\\', x] `shouldBe` Right (Sym x)

    it "parses \"a|b|c\" as (Or (Sym a) (Or (Sym b) (Sym c)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y ->
      forAll validChar $ \z -> parseRegex ['(', x, '|', y, '|', z, ')'] `shouldBe` Right (Or (Or (Sym x) (Sym y)) (Sym z))

    it "parses \"[abc]\" as (Or (Or (Sym a) (Sym b)) (Sym c))" $
      forAll (vectorOf 3 validChar) $ \xs -> parseRegex ('[' : xs ++ "]") `shouldBe` Right (Or (Or (Sym $ head xs) (Sym $ xs !! 1)) (Sym $ xs !! 2))

    it "parses \"abc\" as (Seq (Seq (Sym a) (Sym b)) (Sym c))" $
      forAll (vectorOf 3 validChar) $ \xs -> parseRegex xs `shouldBe` Right (Seq (Seq (Sym $ head xs) (Sym $ xs !! 1)) (Sym $ xs !! 2))

    it "parses \"a*\" as (Kleene (Sym a))" $
      forAll validChar $ \x -> parseRegex [x, '*'] `shouldBe` Right (Kleene (Sym x))

    it "parses \"a?\" as (Or Empty (Sym a))" $
      forAll validChar $ \x -> parseRegex [x, '?'] `shouldBe` Right (Or Empty (Sym x))

    it "parses \"ab*\" as (Seq (Sym a) (Kleene (Sym a)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y ->
        let ast = Seq (Sym x) (Kleene (Sym y))
        in parseRegex [x, y, '*'] `shouldBe` Right ast

    it "parses \"[ab]*\" as (Kleene (Or (Sym a) (Sym b)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y ->
        let ast = Kleene (Or (Sym x) (Sym y))
        in parseRegex ['[', x, y, ']', '*'] `shouldBe` Right ast

    it "parses \"([ab]c|.d)*\" as (Kleene (Or (Seq (Or (Sym a) (Sym b)) (Sym y)) (Seq Dot (Sym d)))" $
      forAll validChar $ \x ->
      forAll validChar $ \y ->
      forAll validChar $ \w ->
      forAll validChar $ \z ->
        let ast = Kleene (Or (Seq (Or (Sym x) (Sym y)) (Sym w)) (Seq Dot (Sym z)))
        in parseRegex ['(', '[', x, y, ']', w, '|', '.', z, ')', '*'] `shouldBe` Right ast

main :: IO ()
main = hspec spec
