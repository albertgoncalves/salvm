module Ast where

import Control.Applicative (many, some, (<|>))
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Parser (Line, Parser (..), char, end, satisfy, sepBy, string)

data Type
  = TBool
  | TFloat
  | TInt
  deriving (Eq, Show)

data Lit
  = LBool Bool
  | LFloat Float
  | LInt Int
  deriving (Eq, Show)

data Expr
  = ECall Expr [Expr]
  | ELit (Line Lit)
  | EType (Line Type)
  | EVar (Line String)
  deriving (Eq, Show)

data Stmt
  = SAssign Expr Expr
  | SDecl (Line Type) Expr
  | SEffect Expr
  | SFn Expr [Stmt] [Stmt]
  | SIfElse Expr [Stmt] [Stmt]
  | SIf Expr [Stmt]
  | SLoop [Stmt]
  | SRet Expr
  deriving (Eq, Show)

comment :: Parser (Line ())
comment =
  void
    <$> ( string "--"
            <* many (satisfy (/= '\n'))
            <* ((void <$> char '\n') <|> (pure <$> end))
        )

space :: Parser (Line ())
space = void <$> satisfy isSpace

anySpace :: Parser [Line ()]
anySpace = many (comment <|> space)

someSpace :: Parser [Line ()]
someSpace = some (comment <|> space)

sepAnySpace :: Parser a -> Parser [a]
sepAnySpace p = anySpace *> sepBy anySpace p <* anySpace

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

ret :: Parser Stmt
ret = SRet <$> (string "return" *> someSpace *> expr <* semicolon)

assign :: Parser Stmt
assign =
  SAssign
    <$> (var <* someSpace <* char '=' <* someSpace) <*> (expr <* semicolon)

if' :: Parser Stmt
if' =
  SIf <$> (string "if" *> someSpace *> expr) <*> (lBrace *> stmts <* rBrace)

ifelse :: Parser Stmt
ifelse =
  SIfElse
    <$> (string "if" *> someSpace *> expr)
    <*> (lBrace *> stmts <* rBrace <* string "else")
    <*> ( (lBrace *> stmts <* rBrace)
            <|> ((: []) <$> (someSpace *> (ifelse <|> if')))
        )

loop :: Parser Stmt
loop = SLoop <$> (string "loop" *> lBrace *> stmts <* rBrace)

decl :: Parser Stmt
decl = SDecl <$> (type' <* someSpace) <*> var

fn :: Parser Stmt
fn =
  SFn
    <$> (type' *> someSpace *> var)
    <*> (lParen *> sepBy comma decl <* rParen)
    <*> (lBrace *> stmts <* rBrace)

effect :: Parser Stmt
effect = SEffect <$> (expr <* semicolon)

stmt :: Parser Stmt
stmt =
  ret
    <|> ifelse
    <|> if'
    <|> loop
    <|> assign
    <|> (decl <* semicolon)
    <|> effect

stmts :: Parser [Stmt]
stmts = sepAnySpace stmt

ast :: Parser [Stmt]
ast = sepAnySpace (fn <|> stmt) <* end