module Parse
  ( parseRegex
  , reserved
  ) where

import Control.Applicative ((<*), (*>), liftA, liftA2)
import Text.Parsec

import Regex

type Parser = Parsec String ()

parseRegex :: String -> Either ParseError Regex
parseRegex = parse regex "Regex Matcher"

regex :: Parser Regex
regex = liftA (foldl1 Seq) $ many1 (try alternative <|> try kleene <|> symbol)

symbol :: Parser Regex
symbol = liftA Symbol $ (try (char '\\') *> oneOf reserved) <|> noneOf reserved

reserved :: String
reserved = "()|\\*"

alternative :: Parser Regex
alternative = liftA2 Or (char '(' *> regex) $ char '|' *> regex <* char ')'

kleene :: Parser Regex
kleene = liftA Kleene $ (char '(' *> regex <* char ')' <|> symbol) <* char '*'
