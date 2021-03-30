module Ast where

import Control.Applicative (many, some, (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Parser (Line, Parser (..), char, end, satisfy, sepBy, string)

data Type
  = TBool
  | TInt
  | TFloat
  deriving (Show)

data Lit
  = LInt Int
  | LFloat Float
  | LBool Bool
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
  | SIf Expr [Stmt]
  | SIfElse Expr [Stmt] [Stmt]
  | SLoop [Stmt]
  | SRet Expr
  | SFn Expr [Stmt] [Stmt]
  deriving (Show)

anySpace :: Parser [Line Char]
anySpace = many (satisfy isSpace)

underscore :: Parser (Line Char)
underscore = char '_'

comma :: Parser (Line Char)
comma = anySpace *> char ',' <* anySpace

semicolon :: Parser (Line Char)
semicolon = anySpace *> char ';'

lBrace :: Parser (Line Char)
lBrace = anySpace *> char '{' <* anySpace

rBrace :: Parser (Line Char)
rBrace = anySpace *> char '}' <* anySpace

lParen :: Parser (Line Char)
lParen = char '(' <* anySpace

rParen :: Parser (Line Char)
rParen = anySpace *> char ')'

digits :: Parser [Line Char]
digits = some (satisfy isDigit)

decimals :: Parser [Line Char]
decimals = (++) <$> digits <*> ((:) <$> char '.' <*> digits)

maybeMinus :: Parser [Line Char] -> Parser [Line Char]
maybeMinus p = ((:) <$> char '-' <*> p) <|> p

type' :: Parser (Line Type)
type' =
  ((TBool <$) <$> string "bool")
    <|> ((TInt <$) <$> string "i32")
    <|> ((TFloat <$) <$> string "f32")

float :: Parser (Line Lit)
float = (LFloat . read <$>) . sequenceA <$> maybeMinus decimals

int :: Parser (Line Lit)
int = (LInt . read <$>) . sequenceA <$> maybeMinus digits

true :: Parser (Line Lit)
true = (LBool True <$) <$> string "true"

false :: Parser (Line Lit)
false = (LBool False <$) <$> string "false"

lit :: Parser Expr
lit = ELit <$> (float <|> int <|> true <|> false)

var :: Parser Expr
var =
  EVar . sequenceA
    <$> ( (:)
            <$> (satisfy isAlpha <|> underscore)
            <*> many (satisfy isAlphaNum <|> underscore)
        )

call :: Parser Expr
call = ECall <$> var <*> (lParen *> sepBy comma expr <* rParen)

expr :: Parser Expr
expr = (EType <$> type') <|> lit <|> call <|> var

assign :: Parser Stmt
assign =
  SAssign <$> (var <* anySpace <* char '=' <* anySpace) <*> (expr <* semicolon)

if' :: Parser Stmt
if' = SIf <$> (string "if" *> anySpace *> expr) <*> (lBrace *> stmts <* rBrace)

ifelse :: Parser Stmt
ifelse =
  SIfElse
    <$> (string "if" *> anySpace *> expr)
    <*> (lBrace *> stmts <* rBrace <* string "else")
    <*> (lBrace *> stmts <* rBrace)

loop :: Parser Stmt
loop = SLoop <$> (string "loop" *> lBrace *> stmts <* rBrace)

decl :: Parser Stmt
decl = SDecl <$> (type' <* anySpace) <*> var

effect :: Parser Stmt
effect = SEffect <$> (expr <* semicolon)

ret :: Parser Stmt
ret = SRet <$> (string "return" *> anySpace *> expr <* semicolon)

fn :: Parser Stmt
fn =
  SFn
    <$> (type' *> anySpace *> var)
    <*> (lParen *> sepBy comma decl <* rParen)
    <*> (lBrace *> stmts <* rBrace)

stmt :: Parser Stmt
stmt =
  ret
    <|> assign
    <|> ifelse
    <|> if'
    <|> loop
    <|> (decl <* semicolon)
    <|> effect

stmts :: Parser [Stmt]
stmts = anySpace *> sepBy anySpace stmt <* anySpace

ast :: Parser [Stmt]
ast = anySpace *> sepBy anySpace (fn <|> stmt) <* anySpace <* end
