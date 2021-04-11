module Ast where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace)
import Data.Text (Text, pack)
import Parser
  ( Parser (..),
    Pos,
    char,
    end,
    many,
    many1,
    satisfy,
    string,
    until',
  )

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _ = False

comment :: Parser (Pos ())
comment =
  void
    <$> ( string "--"
            <* until' isNewline
            <* ((void <$> satisfy isNewline) <|> end)
        )

space :: Parser (Pos ())
space = void . sequenceA <$> many1 (satisfy isSpace)

spaceOrComments :: Parser (Pos ())
spaceOrComments = void . sequenceA <$> many1 (space <|> comment)

digits :: Parser [Pos Char]
digits = many1 (satisfy isDigit)

decimal :: Parser [Pos Char]
decimal = (++) <$> digits <*> ((:) <$> char '.' <*> digits)

signed :: Parser [Pos Char] -> Parser [Pos Char]
signed p = ((:) <$> char '-' <*> p) <|> p

int :: Parser (Pos Int)
int = (read <$>) . sequenceA <$> signed digits

float :: Parser (Pos Float)
float = (read <$>) . sequenceA <$> signed decimal

bool :: Parser (Pos Bool)
bool =
  (const True <$>) <$> string "true" <|> (const False <$>) <$> string "false"

underscore :: Parser (Pos Char)
underscore = char '_'

ident :: Parser (Pos Text)
ident =
  (pack <$>) . sequenceA
    <$> ( (:)
            <$> (satisfy isLower <|> underscore)
            <*> many (satisfy isAlphaNum <|> underscore)
        )

doubleQuote :: Parser (Pos Char)
doubleQuote = char '"'

stringLiteral :: Parser (Pos Text)
stringLiteral =
  (\(n, _) (_, xs) -> (n, pack xs))
    <$> doubleQuote <*> (until' (== '"') <* doubleQuote)
