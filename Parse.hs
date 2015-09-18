module Parse
  ( parseRegex
  , reserved
  ) where

import Control.Applicative ((<*), (*>), liftA, liftA2)
import Text.Parsec (char, many1, noneOf, oneOf, parse, Parsec, ParseError, try, (<|>))

import Regex

type Parser = Parsec String ()

parseRegex :: String -> Either ParseError Regex
parseRegex = parse regex "Regex Matcher"

regex :: Parser Regex
regex = liftA (foldr1 Seq) $ many1 (try alternative <|> try kleene <|> try optional <|> term)

term :: Parser Regex
term = char '(' *> regex <* char ')' <|> symbol <|> characterClass

alternative :: Parser Regex
alternative = liftA2 Or (char '(' *> regex) $ char '|' *> regex <* char ')'

characterClass :: Parser Regex
characterClass = liftA (foldr1 Or) $ char '[' *> many1 symbol <* char ']'

reserved :: String
reserved = "()[]|\\*?"

optional :: Parser Regex
optional = liftA (Or Regex.Empty) $ term <* char '?'

symbol :: Parser Regex
symbol = liftA Symbol $ (try (char '\\') *> oneOf reserved) <|> noneOf reserved

kleene :: Parser Regex
kleene = liftA Kleene $ term <* char '*'
