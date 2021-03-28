import Control.Applicative (many, optional, some, (<|>))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Parser
  ( Input (..),
    ParseError (..),
    Parser (..),
    char,
    satisfy,
    sepBy,
    string,
  )

data Type
  = TInt
  | TFloat
  deriving (Show)

data Lit
  = LInt Int
  | LFloat Float
  deriving (Show)

data Expr
  = EType Type
  | ELit Lit
  | EVar String
  | ECall Expr [Expr]
  deriving (Show)

data Stmt
  = SEffect Expr
  | SDecl Type Expr
  | SAssign Expr Expr
  deriving (Show)

anySpace :: Parser String
anySpace = many (satisfy isSpace)

underscore :: Parser Char
underscore = char '_'

comma :: Parser Char
comma = anySpace *> char ',' <* anySpace

semicolon :: Parser Char
semicolon = anySpace *> char ';'

digits :: Parser String
digits = some (satisfy isDigit)

decimals :: Parser String
decimals = (\l m r -> l ++ (m : r)) <$> digits <*> char '.' <*> digits

maybeMinus :: Parser String -> Parser String
maybeMinus = ((\x xs -> maybe xs (: xs) x) <$> optional (char '-') <*>)

type' :: Parser Type
type' = (TInt <$ string "i32") <|> (TFloat <$ string "f32")

lit :: Parser Lit
lit =
  (LFloat . read <$> maybeMinus decimals)
    <|> (LInt . read <$> maybeMinus digits)

var :: Parser Expr
var =
  EVar
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

-- stmt :: Parser Stmt
-- stmt = Parser $ \i -> do
--   (i', x) <-
--     parse
--       ( assign
--           <|> (SDecl <$> (type' <* anySpace) <*> (var <* semicolon))
--           <|> (SEffect <$> (expr <* semicolon))
--       )
--       i
--   return (i', x)

stmt :: Parser Stmt
stmt =
  assign
    <|> (SDecl <$> (type' <* anySpace) <*> (var <* semicolon))
    <|> (SEffect <$> (expr <* semicolon))

end :: Parser ()
end = Parser $ \i ->
  if null (rest i)
    then pure (i, ())
    else Left $ ParseError $ line i

stmts :: Parser [Stmt]
stmts = anySpace *> sepBy anySpace stmt <* anySpace <* end

main :: IO ()
main =
  mapM_
    (print . parse stmts . Input 1)
    [ "",
      "1234;",
      "-1234;",
      "123.4567;",
      "-123.4567;",
      "\n\nx;",
      "foo;",
      "_foo123_;",
      "\nf();",
      "f(x);",
      "f(x, y);",
      "f( x , y );",
      --
      "\n\
      \i32 x;\n\
      \\n\
      \x 1;",
      --
      "\n\nx = f(1, 2, 3);",
      --
      "\n\
      \\n\
      \\n\
      \x = f(\n\
      \1 2, 3);",
      --
      "\n\
      \\n\
      \i32 x;\n\
      \i32 y;\n\
      \f32 z;\n\
      \x = 1;\n\
      \y = f(x, x);\n\
      \z = 0.0123;\n\
      \print(x);"
    ]
