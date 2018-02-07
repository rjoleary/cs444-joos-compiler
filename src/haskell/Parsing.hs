{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This code here is based on the paper Monadic Parsing in Haskell
-- (http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) and the improvements
-- made to it in
-- http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/

module Parsing where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Control.Monad
import Data.Char (isSpace, isDigit, ord)

newtype Parser a = Parser (StateT String Maybe a) deriving (Functor, Applicative, Alternative)

instance Monad Parser where
  a >>= f = Parser $ StateT $ \s -> do
    (a', s') <- runParser a s
    runParser (f a') s'

runParser :: Parser a -> String -> Maybe (a, String)
runParser (Parser s) = runStateT s

anyChar :: Parser Char
anyChar = Parser . StateT $ \s -> case s of
  [] -> empty
  (c:cs) -> pure (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
  c <- anyChar
  guard $ pred c
  pure c

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

notChar :: Char -> Parser Char
notChar = satisfy . (\x -> not . (== x))

-- TODO Broken. This only works if the string we're parsing is
--      at least as long as c:cs
notString :: String -> Parser String
notString [] = pure []
notString (c:cs) = (:) <$> notChar c <*> notString cs

apply :: Parser a -> String -> Maybe (a, String)
apply p = runParser p
