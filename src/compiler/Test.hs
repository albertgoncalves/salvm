{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Ast
  ( bool,
    charLiteral,
    comment,
    float,
    ident,
    int,
    space,
    spaceOrComments,
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
        parseWith comment "--\n",
        Consumed $ Right ((Min 1, ()), Input 3 "")
      ),
      ( FILE_LINE,
        parseWith comment " --\n",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith comment "-- foo bar baz\n",
        Consumed $ Right ((Min 1, ()), Input 15 "")
      ),
      ( FILE_LINE,
        parseWith comment "-- foo bar baz",
        Consumed $ Right ((Min 1, ()), Input 14 "")
      ),
      ( FILE_LINE,
        parseWith comment "-- foo bar baz\n\n",
        Consumed $ Left 15
      ),
      ( FILE_LINE,
        parseWith comment "-- foo bar baz\nx",
        Consumed $ Left 15
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
        parseWith spaceOrComments "",
        Empty $ Left 0
      ),
      ( FILE_LINE,
        parseWith spaceOrComments "  \n-- ...\n\n  -- ??? \n\n ",
        Consumed $ Right ((Min 1, ()), Input 23 "")
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
        parseWith int "-1234",
        Consumed $ Right ((Min 1, -1234), Input 5 "")
      ),
      ( FILE_LINE,
        parseWith int "-123x4",
        Consumed $ Left 4
      ),
      ( FILE_LINE,
        parseWith int " -1234",
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
        parseWith float "-1234.56789",
        Consumed $ Right ((Min 1, -1234.56789), Input 11 "")
      ),
      ( FILE_LINE,
        parseWith float "-1234.x56789",
        Consumed $ Left 6
      ),
      ( FILE_LINE,
        parseWith float " -1234.56789",
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
