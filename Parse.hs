module Parse
  ( parseRegex
  , reserved
  ) where

import           Control.Applicative ((<|>), (<$), (<*), (*>), liftA, liftA2, pure)
import           Text.Parsec         (between, char, eof, many1, noneOf, oneOf, parse, Parsec, ParseError, try)
import           Text.Parsec.Expr    (Assoc(..), buildExpressionParser, Operator(..))

import Regex

type Parser = Parsec String ()

parseRegex :: String -> Either ParseError Regex
parseRegex = parse regex "Regex Matcher"

{-
term :: Parser Regex
term = (char '(' *> regex <* char ')') <|> dot <|> symbol <|> characterClass

alternative :: Parser Regex
alternative = liftA2 Or (regex <* char '|') regex
-}

characterClass :: Parser Regex
characterClass = liftA (foldr1 Or) $ char '[' *> many1 symbol <* char ']'

reserved :: String
reserved = "()[]|\\*?.+"

{-
optional :: Parser Regex
optional = liftA (Or Regex.Empty) $ term <* char '?'
-}

dot :: Parser Regex
dot = Dot <$ char '.'

symbol :: Parser Regex
symbol = liftA Symbol $ (try (char '\\') *> oneOf reserved) <|> noneOf reserved

{-
kleene :: Parser Regex
kleene = liftA Kleene $ term <* char '*'
-}

regex :: Parser Regex
regex = buildExpressionParser ops atom
  where ops =   [ [ Postfix (Kleene <$ char '*')
                  , Postfix ((\t -> Seq t (Kleene t)) <$ char '+')
                  , Postfix ((\t -> Or Empty t) <$ char '?')
                  ]
                , [ Infix (pure Seq) AssocLeft
                  ]
                , [ Infix (Or <$ char '|') AssocLeft
                  ]
                ]


atom = literal <|> characterClass <|> parens regex

literal = dot <|> symbol

parens = between (char '(') $ char ')'

