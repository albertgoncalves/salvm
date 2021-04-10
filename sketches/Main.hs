module Main where

import Test (tests)

main :: IO ()
main = do
  tests
  putStrLn "Done!"
