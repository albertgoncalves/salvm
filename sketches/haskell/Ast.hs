module Ast where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Parser (Parser (..), Pos, char, end, many1, satisfy, string, until')

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
