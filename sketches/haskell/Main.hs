module Main where

import Ast (ast)
import Parser (Input (..), parse)

main :: IO ()
main =
  mapM_
    (print . parse ast . Input 1)
    [ "",
      "1234;\n\n\n",
      "-1234;",
      "123.4567;",
      "-123.4567;",
      "\n\nx;\n",
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
      "\n\nx = f(1, 2, 3);\n",
      --
      "\n\
      \\n\
      \\n\
      \x = f(\n\
      \1, 2, \n\
      \3);\n",
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
