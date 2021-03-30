{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Semigroup (Min (..))

newtype ParseError = ParseError Int
  deriving (Show)

instance Alternative (Either ParseError) where
  empty = Left $ ParseError 1
  (Left _) <|> x = x
  x <|> _ = x

data Input = Input
  { line :: Int,
    rest :: String
  }
  deriving (Show)

type Line a = (Min Int, a)

newtype Parser a = Parser
  { parse :: Input -> Either ParseError (Input, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \i -> do
    (i', x) <- p i
    return (i', f x)

instance Applicative Parser where
  pure x = Parser $ \i -> pure (i, x)
  (Parser p1) <*> (Parser p2) = Parser $ \i -> do
    (i', f) <- p1 i
    (i'', a) <- p2 i'
    return (i'', f a)

instance Alternative Parser where
  empty = Parser $ \i -> Left $ ParseError $ line i
  (Parser p1) <|> (Parser p2) = Parser $ \i -> p1 i <|> p2 i

adv :: Input -> Maybe (Char, Input)
adv (Input _ "") = Nothing
adv (Input l ('\n' : xs)) = Just ('\n', Input (l + 1) xs)
adv (Input l (x : xs)) = Just (x, Input l xs)

satisfy :: (Char -> Bool) -> Parser (Line Char)
satisfy f = Parser $ \i ->
  let l = Left $ ParseError $ line i
   in case adv i of
        Just (x, i') ->
          if f x
            then Right (i', (Min $ line i, x))
            else l
        Nothing -> l

char :: Char -> Parser (Line Char)
char = satisfy . (==)

string :: String -> Parser (Line String)
string = (sequenceA <$>) . traverse char

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

end :: Parser ()
end = Parser $ \i ->
  if null (rest i)
    then pure (i, ())
    else Left $ ParseError $ line i
