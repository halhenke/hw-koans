module Koan.Parser where

import Control.Applicative
import Data.Char
import Data.List
import Data.Semigroup

import Prelude hiding (fail)

enrolled :: Bool
enrolled = True

data ParseResult a
  = ParseSuccess String a
  | ParseFailure String
  deriving (Eq, Show)

instance Functor ParseResult where
  fmap f (ParseSuccess s m) = ParseSuccess s (f m)
  fmap f (ParseFailure s) = ParseFailure s

newtype Parser a = Parser
  { runParser :: String -> ParseResult a
  }

fail :: String -> Parser a
fail message = Parser $ const (ParseFailure message)
-- fail message = Parser $ const $ ParseFailure message

instance Functor Parser where
  -- (String -> Int) -> Parser String -> Parser Int
  -- fmap f (Parser rp) = Parser (f . rp)
  -- fmap f (Parser rp) = Parser (fmap f rp)
  -- fmap f (Parser rp) = Parser $ (fmap <$> fmap) f rp
  fmap f (Parser rp) = Parser (\s -> f <$> rp s)

instance Applicative Parser where
  pure a  = Parser (\s -> ParseSuccess s a)
  -- pure a  = Parser (\s -> ParseSuccess s a)
  (<*>) (Parser pf) (Parser pa) = Parser $ \s -> case (pf s) of
    (ParseSuccess str a_to_b) -> case (pa str) of
      (ParseSuccess str an_a) -> ParseSuccess str (a_to_b an_a)
      (ParseFailure str) -> ParseFailure str
    (ParseFailure str) -> ParseFailure str
    -- otherwise -> expression

instance Alternative Parser where
  empty = fail "empty"
  Parser pa <|> Parser pb = Parser $ \s -> case pa s of
    (ParseSuccess str a) -> (ParseSuccess str a)
    (ParseFailure str) -> pb str

satisfy :: (Char -> Bool) -> Parser Char
satisfy run = let
  go [] = ParseFailure "Incomplete"
  go (c:cs) = if (run c)
    then ParseSuccess cs c
    else ParseFailure "satisfyWith"
  in Parser go

char :: Char -> Parser Char
char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

anyChar :: Parser Char
anyChar = satisfy (const True)

skip :: (Char -> Bool) -> Parser ()
skip p = error "TODO: Implement skip"

peekChar :: Parser (Maybe Char)
peekChar = error "TODO: Implement peekChar"

peekChar' :: Parser Char
peekChar' = error "TODO: Implement peekChar'"

digit :: Parser Char
digit = error "TODO: Implement digit"

letter :: Parser Char
letter = error "TODO: Implement letter"

space :: Parser Char
space = error "TODO: Implement space"

string :: String -> Parser String
string = error "TODO: Implement string"

doubleQuoted :: Parser String
doubleQuoted = error "TODO: Implement doubleQuoted"
