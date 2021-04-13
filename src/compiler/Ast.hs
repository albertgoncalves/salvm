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
    (<$$>),
  )

data Op
  = OpAdd
  | OpSub
  deriving (Eq, Show)

data Expr
  = EBool (Pos Bool)
  | EChar (Pos Char)
  | EFloat (Pos Float)
  | EIdent (Pos Text)
  | EInt (Pos Int)
  | EStr (Pos Text)
  | EUnOp (Pos Op) Expr
  | EBinOp Expr (Pos Op) Expr
  deriving (Eq, Show)

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _ = False

comment :: Parser (Pos ())
comment =
  void
    <$> ( string "--"
            <* many (satisfy $ not . isNewline)
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

int :: Parser (Pos Int)
int = (read <$>) . sequenceA <$> digits

float :: Parser (Pos Float)
float = (read <$>) . sequenceA <$> decimal

bool :: Parser (Pos Bool)
bool = (const True <$$> string "true") <|> (const False <$$> string "false")

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

squeeze :: Pos a -> Pos b -> Pos b
squeeze (p, _) (_, x) = (p, x)

stringLiteral :: Parser (Pos Text)
stringLiteral =
  (pack <$$>) . squeeze
    <$> doubleQuote <*> (sequenceA <$> many p <* doubleQuote)
  where
    p = (const '"' <$$> string "\\\"") <|> satisfy (/= '"')

singleQuote :: Parser (Pos Char)
singleQuote = char '\''

charLiteral :: Parser (Pos Char)
charLiteral = singleQuote *> satisfy (const True) <* singleQuote

exprStart :: Parser Expr
exprStart =
  (char '(' *> expr <* char ')')
    <|> EUnOp <$> (const OpSub <$$> char '-') <*> expr
    <|> EBool <$> bool
    <|> EChar <$> charLiteral
    <|> EFloat <$> float
    <|> EIdent <$> ident
    <|> EInt <$> int
    <|> EStr <$> stringLiteral

op :: Parser (Pos Op)
op = (const OpAdd <$$> char '+') <|> (const OpSub <$$> char '-')

exprEnd :: Parser (Pos Op, Expr)
exprEnd = (,) <$> (spaceOrComments *> op) <*> (spaceOrComments *> expr)

-- NOTE: See `https://github.com/glebec/left-recursion`.
expr :: Parser Expr
expr = (uncurry . EBinOp <$> exprStart <*> exprEnd) <|> exprStart
