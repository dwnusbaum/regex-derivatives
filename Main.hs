module Main where

import Regex

main :: IO ()
main = print $ matches regex "aabbc"
  where regex = Seq (Seq (Kleene (Symbol 'a')) (Kleene (Symbol 'b'))) (Kleene (Symbol 'c'))

