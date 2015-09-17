module RegexSpec where

import Test.Hspec
import Test.QuickCheck
import Regex

spec :: Spec
spec = do
  describe "Regex.matches" $ do
    it "matches a string against a regex" $ do
      let regex = Or (Symbol 'a') (Symbol 'b')
      matches regex "a" `shouldBe` True

main :: IO ()
main = hspec spec
