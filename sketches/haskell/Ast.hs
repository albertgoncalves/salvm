module Ast where

import Control.Applicative (many, some, (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Parser (Line, Parser (..), char, end, satisfy, sepBy, string)

data Type
  = TInt
  | TFloat
  deriving (Show)

data Lit
  = LInt Int
  | LFloat Float
  deriving (Show)

data Expr
  = EType (Line Type)
  | ELit (Line Lit)
  | EVar (Line String)
  | ECall Expr [Expr]
  deriving (Show)

data Stmt
  = SEffect Expr
  | SDecl (Line Type) Expr
  | SAssign Expr Expr
  deriving (Show)

anySpace :: Parser [Line Char]
anySpace = many (satisfy isSpace)

underscore :: Parser (Line Char)
underscore = char '_'

comma :: Parser (Line Char)
comma = anySpace *> char ',' <* anySpace

semicolon :: Parser (Line Char)
semicolon = anySpace *> char ';'

digits :: Parser [Line Char]
digits = some (satisfy isDigit)

decimals :: Parser [Line Char]
decimals = (++) <$> digits <*> ((:) <$> char '.' <*> digits)

maybeMinus :: Parser [Line Char] -> Parser [Line Char]
maybeMinus p = ((:) <$> char '-' <*> p) <|> p

type' :: Parser (Line Type)
type' = ((TInt <$) <$> string "i32") <|> ((TFloat <$) <$> string "f32")

float :: Parser (Line Lit)
float = (LFloat . read <$>) . sequenceA <$> maybeMinus decimals

int :: Parser (Line Lit)
int = (LInt . read <$>) . sequenceA <$> maybeMinus digits

lit :: Parser (Line Lit)
lit = float <|> int

var :: Parser Expr
var =
  EVar . sequenceA
    <$> ( (:)
            <$> (satisfy isAlpha <|> underscore)
            <*> many (satisfy isAlphaNum <|> underscore)
        )

call :: Parser Expr
call =
  ECall
    <$> var
    <*> (char '(' *> anySpace *> sepBy comma expr <* anySpace <* char ')')

expr :: Parser Expr
expr = call <|> (EType <$> type') <|> (ELit <$> lit) <|> var

assign :: Parser Stmt
assign =
  SAssign <$> (var <* anySpace <* char '=' <* anySpace) <*> (expr <* semicolon)

stmt :: Parser Stmt
stmt =
  assign
    <|> (SDecl <$> (type' <* anySpace) <*> (var <* semicolon))
    <|> (SEffect <$> (expr <* semicolon))

ast :: Parser [Stmt]
ast = anySpace *> sepBy anySpace stmt <* anySpace <* end
