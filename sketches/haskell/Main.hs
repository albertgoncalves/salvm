{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ast (Expr (..), Lit (..), Stmt (..), Type (..), ast)
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Parser (Input (..), ParseError, parse)
import System.Exit (exitFailure)

test :: (Eq a, Show a) => [(a, a)] -> IO ()
test [] = putChar '\n'
test ((l, r) : xs)
  | l == r = putChar '.' >> test xs
  | otherwise =
    putChar '\n'
      >> putStrLn (" -> `" ++ show l ++ "`\n -> `" ++ show r ++ "`")
      >> exitFailure

prep ::
  (Text, Either ParseError [Stmt]) ->
  (Either ParseError [Stmt], Either ParseError [Stmt])
prep (l, r) = ((snd <$>) $ parse ast $ Input 1 l, r)

main :: IO ()
main =
  test $
    map
      prep
      [ ("", Right []),
        ("true;", Right [SEffect $ ELit (Min 1, LBool True)]),
        ("--\nfalse; -- ...", Right [SEffect $ ELit (Min 2, LBool False)]),
        ("\n\n1234;\n", Right [SEffect $ ELit (Min 3, LInt 1234)]),
        ("-1234;\n\n\n", Right [SEffect $ ELit (Min 1, LInt $ -1234)]),
        ("\n123.4567;", Right [SEffect $ ELit (Min 2, LFloat 123.4567)]),
        ("-123.4567;", Right [SEffect $ ELit (Min 1, LFloat $ -123.4567)]),
        ("\n\nx;\n", Right [SEffect $ EVar (Min 3, "x")]),
        ("foo;", Right [SEffect $ EVar (Min 1, "foo")]),
        ("_foo123_;", Right [SEffect $ EVar (Min 1, "_foo123_")]),
        ("\nf();", Right [SEffect $ ECall (EVar (Min 2, "f")) []]),
        ( "f(x);",
          Right [SEffect $ ECall (EVar (Min 1, "f")) [EVar (Min 1, "x")]]
        ),
        ( "f(-- x);\ny);",
          Right [SEffect $ ECall (EVar (Min 1, "f")) [EVar (Min 2, "y")]]
        ),
        ( "f(x, y);",
          Right
            [ SEffect $
                ECall
                  (EVar (Min 1, "f"))
                  [ EVar (Min 1, "x"),
                    EVar (Min 1, "y")
                  ]
            ]
        ),
        ( "f( x , true , 1.1);",
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
        ( "\n\
          \i32 x;\n\
          \\n\
          \x = 1;",
          Right
            [ SDecl (Min 2, TInt) (EVar (Min 2, "x")),
              SAssign (EVar (Min 4, "x")) (ELit (Min 4, LInt 1))
            ]
        ),
        ( "\n\nx = f(\n1, \n2, \n3);\n",
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
        ( "\n\
          \\n\
          \i32 x;\n\
          \i32 y;\n\
          \f32 z;\n\
          \x = 1;\n\
          \y = f(x, x);\n\
          \z = 0.0123;\n\
          \print( x );",
          Right
            [ SDecl (Min 3, TInt) (EVar (Min 3, "x")),
              SDecl (Min 4, TInt) (EVar (Min 4, "y")),
              SDecl (Min 5, TFloat) (EVar (Min 5, "z")),
              SAssign (EVar (Min 6, "x")) (ELit (Min 6, LInt 1)),
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
        ( "if true { print(1); }",
          Right
            [ SIf
                (ELit (Min 1, LBool True))
                [ SEffect $
                    ECall (EVar (Min 1, "print")) [ELit (Min 1, LInt 1)]
                ]
            ]
        ),
        ( "if true { print(1); } else { print(0); }",
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
        ( "loop { i32 x; x = rand(); print(x); }",
          Right
            [ SLoop
                [ SDecl (Min 1, TInt) (EVar (Min 1, "x")),
                  SAssign
                    (EVar (Min 1, "x"))
                    (ECall (EVar (Min 1, "rand")) []),
                  SEffect $ ECall (EVar (Min 1, "print")) [EVar (Min 1, "x")]
                ]
            ]
        ),
        ("return -43.21;", Right [SRet $ ELit (Min 1, LFloat $ -43.21)]),
        ("bool x;", Right [SDecl (Min 1, TBool) (EVar (Min 1, "x"))]),
        ( "bool f(f32 x) {\n    print(x); return true;\n}",
          Right
            [ SFn
                (EVar (Min 1, "f"))
                [SDecl (Min 1, TFloat) (EVar (Min 1, "x"))]
                [ SEffect $ ECall (EVar (Min 2, "print")) [EVar (Min 2, "x")],
                  SRet $ ELit (Min 2, LBool True)
                ]
            ]
        ),
        ( "if 2 { } else { if 1 { } else { } }",
          Right
            [ SIfElse
                (ELit (Min 1, LInt 2))
                []
                [SIfElse (ELit (Min 1, LInt 1)) [] []]
            ]
        ),
        ( "if 2 {\n} else if 1 {\n} else if 0 {\n}",
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
        ( "if 2 {\n} -- else if 1 {\n-- }\nelse if 0 {\n}",
          Right
            [SIfElse (ELit (Min 1, LInt 2)) [] [SIf (ELit (Min 4, LInt 0)) []]]
        )
      ]
