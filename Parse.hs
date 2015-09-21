module Parse
  ( parseRegex
  , reserved
  ) where

import Control.Applicative ((<|>), (<$), (<*), (*>), liftA, pure)
import Data.List           (foldl1')
import Text.Parsec         (between, char, eof, many1, noneOf, oneOf, parse, Parsec, ParseError, try)
import Text.Parsec.Expr    (Assoc(..), buildExpressionParser, Operator(..))

import Regex

type Parser = Parsec String ()

-- | Parses a string into a Right regex. If the string is invalid, it returns
-- a Left ParseError.
parseRegex :: String -> Either ParseError Regex
parseRegex = parse (regex <* eof) "Regex Matcher"

-- | The main parser for regexes.  The unary operators have higher precedence
-- than sequencing two regexes, which in turn has higher precedence than
-- alternating two regexes.  The use of buildExpressionParser avoids unbounded
-- left recursion.
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

-- | A single atom of a regex is a literal, a character class, or something in parentheses
atom :: Parser Regex
atom = literal <|> characterClass <|> parens regex
  where parens = between (char '(') $ char ')'

-- | A literal is the dot character or any other character
literal :: Parser Regex
literal = dot <|> symbol

-- | The list of reserved characters.  These characters have meaning to the
-- parser, and so they must be escaped to match against them literally.
reserved :: String
reserved = "()[]|\\*?.+"

-- | Symbols are any character, but if the character is an operator than it
-- must be escaped using a backslash.
symbol :: Parser Regex
symbol = liftA Sym $ (try (char '\\') *> oneOf reserved) <|> noneOf reserved

-- | Matches the dot character
dot :: Parser Regex
dot = Dot <$ char '.'

-- | Matches a character class.  It is desugared into a left associative chain
-- of Or constructors.
characterClass :: Parser Regex
characterClass = liftA (foldl1' Or) $ char '[' *> many1 symbol <* char ']'

