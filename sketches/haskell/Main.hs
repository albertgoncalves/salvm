module Main where

import Ast (ast)
import Data.Text (pack)
import Parser (Input (..), parse)
import Test (test)

main :: IO ()
main = do
  test
  interact (show . parse ast . Input 1 . pack)
