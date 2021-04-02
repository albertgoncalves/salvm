module Ast where

import Control.Applicative (many, some, (<|>))
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isSymbol)
import Parser
  ( Line,
    Parser (..),
    anySepBy,
    char,
    end,
    satisfy,
    someSepBy,
    string,
  )

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
  = EBinOp Expr (Line String) Expr
  | ECall Expr [Expr]
  | ELit (Line Lit)
  | ETuple [Expr]
  | EType (Line Type)
  | EVar (Line String)
  deriving (Eq, Show)

data Stmt
  = SAssign Expr Expr
  | SBreak
  | SDecl (Line Type) Expr
  | SEffect Expr
  | SFn [Line Type] Expr [Stmt] [Stmt] [Stmt]
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
anySpace = many $ comment <|> space

someSpace :: Parser [Line ()]
someSpace = some $ comment <|> space

sepAnySpace :: Parser a -> Parser [a]
sepAnySpace p = anySpace *> p `anySepBy` anySpace <* anySpace

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
digits = some $ satisfy isDigit

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
            <$> satisfy isLower
            <*> many (satisfy isAlphaNum <|> underscore)
        )

call :: Parser Expr
call = ECall <$> var <*> (lParen *> expr `anySepBy` comma) <* rParen

tuple :: Parser Expr
tuple =
  ETuple
    <$> ( (:)
            <$> (lParen *> expr <* comma)
            <*> (expr `someSepBy` comma)
            <* rParen
        )

term :: Parser Expr
term =
  (EType <$> type')
    <|> (lParen *> anySpace *> expr <* anySpace <* rParen)
    <|> tuple
    <|> lit
    <|> call
    <|> var

binOp :: Parser Expr
binOp =
  EBinOp
    <$> (term <* someSpace)
    <*> (sequenceA <$> some (satisfy isSymbol <|> char '-'))
    <*> (someSpace *> expr)

expr :: Parser Expr
expr = binOp <|> term

break :: Parser Stmt
break = SBreak <$ (string "break" <* semicolon)

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
    <$> ( (: [])
            <$> type'
            <|> (lParen *> type' `anySepBy` comma <* rParen)
            <|> pure []
        )
    <*> (anySpace *> var)
    <*> (lParen *> decl `anySepBy` comma <* rParen)
    <*> (lBrace *> sepAnySpace (decl <* semicolon))
    <*> (stmts <* rBrace)

effect :: Parser Stmt
effect = SEffect <$> (expr <* semicolon)

stmt :: Parser Stmt
stmt = ret <|> ifelse <|> if' <|> loop <|> assign <|> effect

stmts :: Parser [Stmt]
stmts = sepAnySpace stmt

ast :: Parser [Stmt]
ast = sepAnySpace fn <* end
