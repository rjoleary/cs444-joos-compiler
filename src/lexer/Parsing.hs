{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This code here is based on the paper Monadic Parsing in Haskell
-- (http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) and the improvements
-- made to it in
-- http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/
module Parsing where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           Data.Char                        (isDigit, isSpace, ord)
import           System.IO

-- Input is tagged with the character's index.
data CharTag = CharTag { tagChar :: Char, tagIdx :: Int }
type StringTag = [CharTag]
invalidIdx = 999999999 :: Int

-- Two characters are equal regardless of their index.
instance Eq CharTag where
    (CharTag x _) == (CharTag y _) = x == y

data NoData =
  NoData
  deriving (Show)

instance Exception NoData

newtype Parser a =
  Parser (StateT StringTag Maybe a)
  deriving (Functor, Applicative, Alternative)

instance Monad Parser where
  a >>= f =
    Parser $
    StateT $ \s -> do
      (a', s') <- runParser a s
      runParser (f a') s'

runParser :: Parser a -> StringTag -> Maybe (a, StringTag)
runParser (Parser s) = runStateT s

anyChar :: Parser CharTag
anyChar =
  Parser . StateT $ \s ->
    case s of
      []     -> empty
      (c:cs) -> pure (c, cs)

satisfy :: (Char -> Bool) -> Parser CharTag
satisfy pred = do
  ct <- anyChar
  let CharTag c _ = ct
  guard $ pred c
  pure ct

char :: Char -> Parser CharTag
char = satisfy . (==)

string :: String -> Parser StringTag
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

notChar :: Char -> Parser CharTag
notChar = satisfy . (\x -> not . (== x))

oneOfChar :: [Char] -> Parser CharTag
oneOfChar l = satisfy (\x -> any (== x) l)

manyTill :: Parser CharTag -> Parser StringTag -> Parser StringTag
manyTill p end = scan
  where
    scan =
      do end
         return []
     <|> do
        c <- p
        cs <- manyTill p end
        return (c : cs)

manyTill1 :: Parser StringTag -> Parser StringTag -> Parser StringTag
manyTill1 p end = scan
  where
    scan =
      do end
         return []
     <|> do
        s1 <- p
        s2 <- manyTill1 p end
        return $ s1 ++ s2

apply :: Parser a -> StringTag -> Maybe (a, StringTag)
apply p = runParser p

maybeToIO :: Maybe a -> IO a
maybeToIO Nothing  = throwIO NoData
maybeToIO (Just x) = return x
