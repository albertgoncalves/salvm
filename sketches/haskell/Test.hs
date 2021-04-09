{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

import Ast (comment, float, int, many1Space)
import Data.Semigroup (Min (..))
import Parser (Consumed (..), Input (..), end, parse)
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

tests :: IO ()
tests = do
  eq
    [ ( FILE_LINE,
        parse (comment <* end) $ Input 0 "-- foo bar baz\n",
        Consumed $ Right ((Min 1, ()), Input 15 "")
      ),
      ( FILE_LINE,
        parse (comment <* end) $ Input 0 "-- foo bar baz",
        Consumed $ Right ((Min 1, ()), Input 14 "")
      ),
      ( FILE_LINE,
        parse (many1Space <* end) $ Input 0 "  \n\n\n  ",
        Consumed $ Right ((Min 1, ()), Input 7 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parse int $ Input 0 "1234",
        Consumed $ Right ((Min 1, 1234), Input 4 "")
      ),
      ( FILE_LINE,
        parse int $ Input 0 "-1234",
        Consumed $ Right ((Min 1, -1234), Input 5 "")
      )
    ]
  eq
    [ ( FILE_LINE,
        parse float $ Input 0 "0.12345",
        Consumed $ Right ((Min 1, 0.12345), Input 7 "")
      ),
      ( FILE_LINE,
        parse float $ Input 0 "-1234.56789",
        Consumed $ Right ((Min 1, -1234.56789), Input 11 "")
      )
    ]
