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
    sepBy,
    string,
    (<$$>),
  )

data Op
  = OpAdd
  | OpSub
  deriving (Eq, Show)

data Expr
  = EBinOp Expr (Pos Op) Expr
  | EBool (Pos Bool)
  | ECall (Pos Text) [Expr]
  | EChar (Pos Char)
  | EFloat (Pos Float)
  | EIdent (Pos Text)
  | EInt (Pos Int)
  | EStr (Pos Text)
  | EUnOp (Pos Op) Expr
  deriving (Eq, Show)

data Stmt
  = SAssign Expr Expr
  | SEffect Expr
  | SIf Expr [Stmt]
  deriving (Eq, Show)

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline _ = False

comment :: Parser (Pos ())
comment = void <$> (char '#' <* p1 <* p2)
  where
    p1 = many (satisfy $ not . isNewline)
    p2 = (void <$> satisfy isNewline) <|> end

space :: Parser (Pos ())
space = void . sequenceA <$> many1 (satisfy isSpace)

manySpaces :: Parser (Pos ())
manySpaces = void . sequenceA <$> many (space <|> comment)

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
ident = (pack <$>) . sequenceA <$> ((:) <$> p1 <*> p2)
  where
    p1 = satisfy isLower <|> underscore
    p2 = many (satisfy isAlphaNum <|> underscore)

doubleQuote :: Parser (Pos Char)
doubleQuote = char '"'

stringLiteral :: Parser (Pos Text)
stringLiteral =
  (\(n, _) (_, x) -> (n, pack x))
    <$> doubleQuote <*> (sequenceA <$> p <* doubleQuote)
  where
    p = many $ (const '"' <$$> string "\\\"") <|> satisfy (/= '"')

singleQuote :: Parser (Pos Char)
singleQuote = char '\''

charLiteral :: Parser (Pos Char)
charLiteral = singleQuote *> satisfy (const True) <* singleQuote

unaryOp :: Parser Expr
unaryOp = EUnOp <$> (const OpSub <$$> char '-') <*> expr

call :: Parser Expr
call = ECall <$> (ident <* char '(' <* manySpaces) <*> (p <* char ')')
  where
    p = (expr <* manySpaces) `sepBy` (char ',' <* manySpaces)

exprHead :: Parser Expr
exprHead =
  (char '(' *> expr <* char ')')
    <|> call
    <|> unaryOp
    <|> EBool <$> bool
    <|> EChar <$> charLiteral
    <|> EFloat <$> float
    <|> EIdent <$> ident
    <|> EInt <$> int
    <|> EStr <$> stringLiteral

op :: Parser (Pos Op)
op = (const OpAdd <$$> char '+') <|> (const OpSub <$$> char '-')

exprTail :: Parser (Pos Op, Expr)
exprTail = (,) <$> (manySpaces *> op) <*> (manySpaces *> expr)

-- NOTE: See `https://github.com/glebec/left-recursion`.
expr :: Parser Expr
expr = (uncurry . EBinOp <$> exprHead <*> exprTail) <|> exprHead

semicolon :: Parser (Pos Char)
semicolon = manySpaces *> char ';'

assign :: Parser Stmt
assign = SAssign <$> (expr <* p) <*> (expr <* semicolon)
  where
    p = manySpaces <* char '=' <* manySpaces

effect :: Parser Stmt
effect = SEffect <$> expr <* semicolon

stmt :: Parser Stmt
stmt = assign <|> effect
