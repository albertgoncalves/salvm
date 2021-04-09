module Ast where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Parser (Parser (..), Pos, char, end, many, many1, satisfy, string)

comment :: Parser (Pos ())
comment =
  void
    <$> ( string "--"
            <* many (satisfy (/= '\n'))
            <* ((void <$> satisfy (== '\n')) <|> end)
        )

many1Space :: Parser (Pos ())
many1Space = void . sequenceA <$> many1 (satisfy isSpace)

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
