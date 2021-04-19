{-# LANGUAGE FlexibleInstances #-}

module Parser where

-- NOTE: See `https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf`.
-- NOTE: See `https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf`.
-- NOTE: See `https://www.researchgate.net/publication/2534571_Parsec_Direct_Style_Monadic_Parser_Combinators_For_The_Real_World`.

import Control.Applicative (Alternative, empty, (<|>))
import Data.Bifunctor (first)
import Data.Semigroup (Min (..))
import Data.Text (Text, null, uncons)
import Prelude hiding (null)

data Input = Input
  { pos :: Int,
    rest :: Text
  }
  deriving (Eq, Show)

type Reply a = Either Int (a, Input)

data Consumed a
  = Consumed (Reply a)
  | Empty (Reply a)
  deriving (Eq, Show)

newtype Parser a = Parser
  { parse :: Input -> Consumed a
  }

type Pos a = (Min Int, a)

instance Functor Consumed where
  fmap f (Consumed x) = Consumed $ first f <$> x
  fmap f (Empty x) = Empty $ first f <$> x

instance Functor Parser where
  fmap f p = Parser $ fmap f . parse p

instance Applicative Parser where
  pure x = Parser $ \i -> Empty (Right (x, i))
  p1 <*> p2 = Parser $ \i ->
    case parse p1 i of
      Consumed (Right (f, i')) ->
        case parse p2 i' of
          Empty x' -> Consumed $ first f <$> x'
          c -> f <$> c
      Consumed (Left n) -> Consumed $ Left n
      Empty (Right (f, i')) -> f <$> parse p2 i'
      Empty (Left n) -> Empty $ Left n

instance Alternative Parser where
  empty = Parser $ \i -> Empty $ Left $ pos i

  -- NOTE: If `p` succeeds without consuming `i` the second alternative is
  -- favored _if_ it consumes `i`. This implements the "longest match" rule.
  p1 <|> p2 = Parser $ \i -> case parse p1 i of
    Empty (Left _) -> parse p2 i
    Empty r ->
      case parse p2 i of
        Empty _ -> Empty r
        c -> c
    c@(Consumed (Left _)) ->
      case parse p2 i of
        c'@(Consumed (Right _)) -> c'
        _ -> c
    c -> c

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = ((f <$>) <$>)

satisfy :: (Char -> Bool) -> Parser (Pos Char)
satisfy f = Parser $ \i ->
  case uncons $ rest i of
    Just (x, rest')
      | f x ->
        let n = pos i + 1
         in Consumed (Right ((Min n, x), Input n rest'))
      | otherwise -> Empty $ Left $ pos i
    Nothing -> Empty $ Left $ pos i

end :: Parser (Pos ())
end = Parser $ \i ->
  if null $ rest i
    then Empty $ Right ((Min $ pos i, ()), i)
    else Empty $ Left (pos i)

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

char :: Char -> Parser (Pos Char)
char = satisfy . (==)

string :: String -> Parser (Pos String)
string = (sequenceA <$>) . traverse char
