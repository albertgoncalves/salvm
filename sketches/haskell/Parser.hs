{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative (Alternative, empty, many, (<|>))

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

-- newtype LineNum a = LineNum (Min Int, a)
--   deriving (Show)
--
-- instance Functor LineNum where
--   fmap f (LineNum (i, x)) = LineNum (i, f x)
--
-- instance Applicative LineNum where
--   pure x = LineNum (mempty, x)
--   (LineNum (l1, f)) <*> (LineNum (l2, x)) = LineNum (l1 <> l2, f x)

adv :: Input -> Either Int (Char, Input)
adv (Input l "") = Left l
adv (Input l ('\n' : xs)) = Right ('\n', Input (l + 1) xs)
adv (Input l (x : xs)) = Right (x, Input l xs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \i ->
  case adv i of
    Right (x, i') ->
      if f x
        then Right (i', x)
        else Left (ParseError $ line i')
    Left l -> Left (ParseError l)

char :: Char -> Parser Char
char = satisfy . (==)

-- string :: String -> Parser String
-- string s = Parser $ \i -> do
--   (i', x) <- parse (traverse char s) i
--   return (i', x)

string :: String -> Parser String
string = traverse char

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []
