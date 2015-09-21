module Parse
  ( parseRegex
  , reserved
  ) where

import           Control.Applicative ((<|>), (<$), (<*), (*>), liftA, pure)
import           Text.Parsec         (between, char, eof, many1, noneOf, oneOf, parse, Parsec, ParseError, try)
import           Text.Parsec.Expr    (Assoc(..), buildExpressionParser, Operator(..))

import Regex

type Parser = Parsec String ()

parseRegex :: String -> Either ParseError Regex
parseRegex = parse (regex <* eof) "Regex Matcher"

regex :: Parser Regex
regex = buildExpressionParser ops atom
  where ops = [ [ Postfix (Kleene <$ char '*')
                , Postfix ((\t -> Seq t (Kleene t)) <$ char '+')
                , Postfix (Or Empty <$ char '?')
                ]
              , [ Infix (pure Seq) AssocLeft
                ]
              , [ Infix (Or <$ char '|') AssocLeft
                ]
              ]

atom :: Parser Regex
atom = literal <|> characterClass <|> parens regex
  where parens = between (char '(') $ char ')'

literal :: Parser Regex
literal = dot <|> symbol

reserved :: String
reserved = "()[]|\\*?.+"

symbol :: Parser Regex
symbol = liftA Symbol $ (try (char '\\') *> oneOf reserved) <|> noneOf reserved

dot :: Parser Regex
dot = Dot <$ char '.'

characterClass :: Parser Regex
characterClass = liftA (foldr1 Or) $ char '[' *> many1 symbol <* char ']'

