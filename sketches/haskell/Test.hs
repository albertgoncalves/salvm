{-# LANGUAGE OverloadedStrings #-}

module Test where

import Ast (Expr (..), Lit (..), Stmt (..), Type (..), ast, stmts)
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Parser (Input (..), ParseError, Parser (..), end, parse)
import System.Exit (exitFailure)

eq :: (Eq a, Show a) => [(a, a)] -> IO ()
eq [] = putChar '\n'
eq ((l, r) : xs)
  | l == r = putChar '.' >> eq xs
  | otherwise =
    putChar '\n'
      >> putStrLn (" -> `" ++ show l ++ "`\n -> `" ++ show r ++ "`")
      >> exitFailure

prep ::
  (Parser a, Text, Either ParseError a) ->
  (Either ParseError a, Either ParseError a)
prep (p, l, r) = ((snd <$>) $ parse p $ Input 1 l, r)

stmts' :: Parser [Stmt]
stmts' = stmts <* end

test :: IO ()
test =
  eq $
    map
      prep
      [ (stmts', "", Right []),
        ( stmts',
          "true;",
          Right [SEffect $ ELit (Min 1, LBool True)]
        ),
        ( stmts',
          "--\nfalse; -- ...",
          Right [SEffect $ ELit (Min 2, LBool False)]
        ),
        (stmts', "\n\n1234;\n", Right [SEffect $ ELit (Min 3, LInt 1234)]),
        (stmts', "-1234;\n\n\n", Right [SEffect $ ELit (Min 1, LInt $ -1234)]),
        ( stmts',
          "\n123.4567;",
          Right [SEffect $ ELit (Min 2, LFloat 123.4567)]
        ),
        ( stmts',
          "-123.4567;",
          Right [SEffect $ ELit (Min 1, LFloat $ -123.4567)]
        ),
        (stmts', "\n\nx;\n", Right [SEffect $ EVar (Min 3, "x")]),
        (stmts', "foo_123_;", Right [SEffect $ EVar (Min 1, "foo_123_")]),
        (stmts', "\nf();", Right [SEffect $ ECall (EVar (Min 2, "f")) []]),
        ( stmts',
          "f(x);",
          Right [SEffect $ ECall (EVar (Min 1, "f")) [EVar (Min 1, "x")]]
        ),
        ( stmts',
          "f(-- x);\ny);",
          Right [SEffect $ ECall (EVar (Min 1, "f")) [EVar (Min 2, "y")]]
        ),
        ( stmts',
          "f(x, y);",
          Right
            [ SEffect $
                ECall
                  (EVar (Min 1, "f"))
                  [ EVar (Min 1, "x"),
                    EVar (Min 1, "y")
                  ]
            ]
        ),
        ( stmts',
          "f( x , true , 1.1);",
          Right
            [ SEffect $
                ECall
                  (EVar (Min 1, "f"))
                  [ EVar (Min 1, "x"),
                    ELit (Min 1, LBool True),
                    ELit (Min 1, LFloat 1.1)
                  ]
            ]
        ),
        ( stmts',
          "\n\nx = f(\n1, \n2, \n3);\n",
          Right
            [ SAssign
                (EVar (Min 3, "x"))
                ( ECall
                    (EVar (Min 3, "f"))
                    [ ELit (Min 4, LInt 1),
                      ELit (Min 5, LInt 2),
                      ELit (Min 6, LInt 3)
                    ]
                )
            ]
        ),
        ( stmts',
          "\n\
          \\n\
          \-- ...\n\
          \-- ...\n\
          \-- ...\n\
          \x = 1;\n\
          \y = f(x, x);\n\
          \z = 0.0123;\n\
          \print( x );",
          Right
            [ SAssign (EVar (Min 6, "x")) (ELit (Min 6, LInt 1)),
              SAssign
                (EVar (Min 7, "y"))
                ( ECall
                    (EVar (Min 7, "f"))
                    [EVar (Min 7, "x"), EVar (Min 7, "x")]
                ),
              SAssign (EVar (Min 8, "z")) (ELit (Min 8, LFloat 0.0123)),
              SEffect $ ECall (EVar (Min 9, "print")) [EVar (Min 9, "x")]
            ]
        ),
        ( stmts',
          "if true { print(1); }",
          Right
            [ SIf
                (ELit (Min 1, LBool True))
                [ SEffect $
                    ECall (EVar (Min 1, "print")) [ELit (Min 1, LInt 1)]
                ]
            ]
        ),
        ( stmts',
          "if true { print(1); } else { print(0); }",
          Right
            [ SIfElse
                (ELit (Min 1, LBool True))
                [ SEffect $
                    ECall (EVar (Min 1, "print")) [ELit (Min 1, LInt 1)]
                ]
                [ SEffect $
                    ECall (EVar (Min 1, "print")) [ELit (Min 1, LInt 0)]
                ]
            ]
        ),
        ( stmts',
          "loop { x = rand(); print(x); }",
          Right
            [ SLoop
                [ SAssign
                    (EVar (Min 1, "x"))
                    (ECall (EVar (Min 1, "rand")) []),
                  SEffect $ ECall (EVar (Min 1, "print")) [EVar (Min 1, "x")]
                ]
            ]
        ),
        ( stmts',
          "return -43.21;",
          Right [SRet $ ELit (Min 1, LFloat $ -43.21)]
        ),
        ( stmts',
          "if 2 { } else { if 1 { } else { } }",
          Right
            [ SIfElse
                (ELit (Min 1, LInt 2))
                []
                [SIfElse (ELit (Min 1, LInt 1)) [] []]
            ]
        ),
        ( stmts',
          "if 2 {\n} else if 1 {\n} else if 0 {\n}",
          Right
            [ SIfElse
                (ELit (Min 1, LInt 2))
                []
                [ SIfElse
                    (ELit (Min 2, LInt 1))
                    []
                    [SIf (ELit (Min 3, LInt 0)) []]
                ]
            ]
        ),
        ( stmts',
          "if 2 {\n} -- else if 1 {\n-- }\nelse if 0 {\n}",
          Right
            [SIfElse (ELit (Min 1, LInt 2)) [] [SIf (ELit (Min 4, LInt 0)) []]]
        ),
        ( ast,
          "main() {\n\
          \    i32 x;\n\
          \    -- ...\n\
          \    x = 1;\n\
          \}",
          Right
            [ SFn
                []
                (EVar (Min 1, "main"))
                []
                [SDecl (Min 2, TInt) (EVar (Min 2, "x"))]
                [SAssign (EVar (Min 4, "x")) (ELit (Min 4, LInt 1))]
            ]
        ),
        ( ast,
          "bool f(f32 x) {\n    print(x); return true;\n}",
          Right
            [ SFn
                [(Min 1, TBool)]
                (EVar (Min 1, "f"))
                [SDecl (Min 1, TFloat) (EVar (Min 1, "x"))]
                []
                [ SEffect $ ECall (EVar (Min 2, "print")) [EVar (Min 2, "x")],
                  SRet $ ELit (Min 2, LBool True)
                ]
            ]
        ),
        ( ast,
          "(i32, i32) f(i32 a, i32 b) {\n\
          \    return (a, b);\n\
          \}",
          Right
            [ SFn
                [(Min 1, TInt), (Min 1, TInt)]
                (EVar (Min 1, "f"))
                [ SDecl (Min 1, TInt) (EVar (Min 1, "a")),
                  SDecl (Min 1, TInt) (EVar (Min 1, "b"))
                ]
                []
                [SRet $ ETuple [EVar (Min 2, "a"), EVar (Min 2, "b")]]
            ]
        ),
        ( ast,
          "f() { (5 > (i - 1)) + 1 < 2; }",
          Right
            [ SFn
                []
                (EVar (Min 1, "f"))
                []
                []
                [ SEffect $
                    EBinOp
                      ( EBinOp
                          (ELit (Min 1, LInt 5))
                          (Min 1, ">")
                          ( EBinOp
                              (EVar (Min 1, "i"))
                              (Min 1, "-")
                              (ELit (Min 1, LInt 1))
                          )
                      )
                      (Min 1, "+")
                      ( EBinOp
                          (ELit (Min 1, LInt 1))
                          (Min 1, "<")
                          (ELit (Min 1, LInt 2))
                      )
                ]
            ]
        )
      ]
