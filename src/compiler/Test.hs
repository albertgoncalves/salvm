{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Ast
  ( Expr (..),
    Op (..),
    Stmt (..),
    bool,
    charLiteral,
    comment,
    expr,
    float,
    ident,
    int,
    manySpaces,
    space,
    stmt,
    stringLiteral,
  )
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Parser (Consumed (..), Input (..), Parser (..), end, parse)
import System.Exit (exitFailure)
import Text.Printf (printf)

#define FILE_LINE __FILE__, __LINE__ + 1

eq :: (Eq a, Show a) => [(String, Int, a, a)] -> IO ()
eq [] = putChar '\n'
eq ((s, i, l, r) : xs)
  | l == r = do
    putChar '.'
    eq xs
  | otherwise = do
    putChar '\n'
    printf "%s:%d\n -> `%s`\n -> `%s`" s i (show l) (show r)
    exitFailure

parseWith :: Parser a -> Text -> Consumed a
parseWith p = parse (p <* end) . Input 0

main :: IO ()
main = do
  eq
    [ ( FILE_LINE,
        parseWith comment "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith comment "#\n",
        Consumed $ Right ((Min 1, ()), Input 2 "")
      ),
      ( FILE_LINE,
        parseWith comment " #\n",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz\n",
        Consumed $ Right ((Min 1, ()), Input 14 "")
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz",
        Consumed $ Right ((Min 1, ()), Input 13 "")
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz\n\n",
        Consumed $ Left 14
      ),
      ( FILE_LINE,
        parseWith comment "# foo bar baz\nx",
        Consumed $ Left 14
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith space "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith space "  \n\n\n  ",
        Consumed $ Right ((Min 1, ()), Input 7 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith manySpaces "",
        Empty $ Right ((mempty, ()), Input 0 "")
      ),
      ( FILE_LINE,
        parseWith manySpaces "  \n# ...\n\n  # ??? \n\n ",
        Consumed $ Right ((Min 1, ()), Input 21 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith int "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith int "1234",
        Consumed $ Right ((Min 1, 1234), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith int "123x4",
        Consumed $ Left 3
      ),
      ( FILE_LINE,
        parseWith int " 1234",
        Empty $ Left 0
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith float "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith float "0.12345",
        Consumed $ Right ((Min 1, 0.12345), Input 7 "")
      ),
      ( FILE_LINE,
        parseWith float "1234.x56789",
        Consumed $ Left 5
      ),
      ( FILE_LINE,
        parseWith float " 1234.56789",
        Empty $ Left 0
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith bool "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith bool "true",
        Consumed $ Right ((Min 1, True), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith bool "false",
        Consumed $ Right ((Min 1, False), Input 5 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith ident "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith ident "xyz",
        Consumed $ Right ((Min 1, "xyz"), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith ident "_xyz",
        Consumed $ Right ((Min 1, "_xyz"), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith ident "_x_Y_Z_0",
        Consumed $ Right ((Min 1, "_x_Y_Z_0"), Input 8 "")
      ),
      ( FILE_LINE,
        parseWith ident "XYZ",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith ident "abc-def",
        Consumed $ Left 3
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith stringLiteral "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"Hello, world!\"",
        Consumed $ Right ((Min 1, "Hello, world!"), Input 15 "")
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"\\\"Hello, world!\\\"\"",
        Consumed $ Right ((Min 1, "\"Hello, world!\""), Input 19 "")
      ),
      ( FILE_LINE,
        parseWith stringLiteral "\"\"",
        Consumed $ Right ((Min 1, ""), Input 2 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith charLiteral "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith charLiteral "'\0'",
        Consumed $ Right ((Min 2, '\0'), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith charLiteral "''",
        Consumed $ Left 2
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith expr "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith expr "true",
        Consumed $ Right (EBool (Min 1, True), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith expr "false",
        Consumed $ Right (EBool (Min 1, False), Input 5 "")
      ),
      ( FILE_LINE,
        parseWith expr "'\0'",
        Consumed $ Right (EChar (Min 2, '\0'), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith expr "'\0\n'",
        Consumed $ Left 2
      ),
      ( FILE_LINE,
        parseWith expr "0.1",
        Consumed $ Right (EFloat (Min 1, 0.1), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith expr "(0.1)",
        Consumed $ Right (EFloat (Min 2, 0.1), Input 5 "")
      ),
      ( FILE_LINE,
        parseWith expr "0.1.",
        Consumed $ Left 3
      ),
      ( FILE_LINE,
        parseWith expr "_foo_bar_0123",
        Consumed $ Right (EIdent (Min 1, "_foo_bar_0123"), Input 13 "")
      ),
      ( FILE_LINE,
        parseWith expr "(_foo_bar_0123)",
        Consumed $ Right (EIdent (Min 2, "_foo_bar_0123"), Input 15 "")
      ),
      ( FILE_LINE,
        parseWith expr "A_foo_bar_0123",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith expr "1234",
        Consumed $ Right (EInt (Min 1, 1234), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith expr "(1234)",
        Consumed $ Right (EInt (Min 2, 1234), Input 6 "")
      ),
      ( FILE_LINE,
        parseWith expr "\"...\\\"?\\\"\"",
        Consumed $ Right (EStr (Min 1, "...\"?\""), Input 10 "")
      ),
      ( FILE_LINE,
        parseWith expr "(-a) + 2",
        Consumed
          ( Right
              ( EBinOp
                  (EUnOp (Min 2, OpSub) (EIdent (Min 3, "a")))
                  (Min 6, OpAdd)
                  (EInt (Min 8, 2)),
                Input 8 ""
              )
          )
      ),
      ( FILE_LINE,
        parseWith expr "(-a) + ?",
        Consumed $ Left 4
      ),
      ( FILE_LINE,
        parseWith expr "2 - -3 + # ?\n2",
        Consumed
          ( Right
              ( EBinOp
                  (EInt (Min 1, 2))
                  (Min 3, OpSub)
                  ( EUnOp
                      (Min 5, OpSub)
                      ( EBinOp
                          (EInt (Min 6, 3))
                          (Min 8, OpAdd)
                          (EInt (Min 14, 2))
                      )
                  ),
                Input 14 ""
              )
          )
      ),
      ( FILE_LINE,
        parseWith expr "2 - -3 + ?",
        Consumed $ Left 6
      ),
      ( FILE_LINE,
        parseWith expr "f()",
        Consumed (Right (ECall (Min 1, "f") [], Input 3 ""))
      ),
      ( FILE_LINE,
        parseWith expr "f( # ...\n)",
        Consumed (Right (ECall (Min 1, "f") [], Input 10 ""))
      ),
      ( FILE_LINE,
        parseWith expr "f(-2,a,true)",
        Consumed
          ( Right
              ( ECall
                  (Min 1, "f")
                  [ EUnOp (Min 3, OpSub) (EInt (Min 4, 2)),
                    EIdent (Min 6, "a"),
                    EBool (Min 8, True)
                  ],
                Input 12 ""
              )
          )
      ),
      ( FILE_LINE,
        parseWith expr "f( -2 , a , true )",
        Consumed
          ( Right
              ( ECall
                  (Min 1, "f")
                  [ EUnOp (Min 4, OpSub) (EInt (Min 5, 2)),
                    EIdent (Min 9, "a"),
                    EBool (Min 13, True)
                  ],
                Input 18 ""
              )
          )
      )
    ]
  eq
    [ ( FILE_LINE,
        parseWith stmt "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith stmt "x = true;",
        Consumed $
          Right
            ( SAssign (EIdent (Min 1, "x")) (EBool (Min 5, True)),
              Input 9 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "x=true  # ...\n    ;",
        Consumed $
          Right
            ( SAssign (EIdent (Min 1, "x")) (EBool (Min 3, True)),
              Input 19 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "f();",
        Consumed $ Right (SEffect (ECall (Min 1, "f") []), Input 4 "")
      ),
      ( FILE_LINE,
        parseWith stmt "f(\n)\n;",
        Consumed $ Right (SEffect (ECall (Min 1, "f") []), Input 6 "")
      ),
      ( FILE_LINE,
        parseWith stmt "if true {\n    f();\n}",
        Consumed $
          Right
            ( SIf (EBool (Min 4, True)) [SEffect (ECall (Min 15, "f") [])],
              Input 20 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "iftrue{f()# ...\n;g()# ...\n;}",
        Consumed $
          Right
            ( SIf
                (EBool (Min 3, True))
                [ SEffect (ECall (Min 8, "f") []),
                  SEffect (ECall (Min 18, "g") [])
                ],
              Input 28 ""
            )
      ),
      ( FILE_LINE,
        parseWith stmt "if true { if false { } }",
        Consumed $
          Right
            ( SIf (EBool (Min 4, True)) [SIf (EBool (Min 14, False)) []],
              Input 24 ""
            )
      )
    ]
