{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative (Alternative, empty, many, (<|>))
import Data.Semigroup (Min (..))
import Data.Text (Text, null, pack, uncons)
import Prelude hiding (null)

newtype ParseError = ParseError Int
  deriving (Eq, Show)

instance Alternative (Either ParseError) where
  empty = Left $ ParseError 1
  (Left _) <|> x = x
  x <|> _ = x

data Input = Input
  { line :: Int,
    rest :: Text
  }
  deriving (Eq, Show)

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
adv (Input l xs) =
  ( \(x, xs') ->
      let l' = case x of
            '\n' -> l + 1
            _ -> l
       in (x, Input l' xs')
  )
    <$> uncons xs

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

string :: String -> Parser (Line Text)
string = ((pack <$>) . sequenceA <$>) . traverse char

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep item = (:) <$> item <*> many (sep *> item) <|> pure []

end :: Parser ()
end = Parser $ \i ->
  if null (rest i)
    then pure (i, ())
    else Left $ ParseError $ line i
