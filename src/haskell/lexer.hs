import Data.Char(isAscii, isDigit, isLetter, isSpace)
import System.Exit
import Control.Monad
import TokenTypes
import Parsing
import Control.Applicative
import Control.Exception

data Token = CharToken Char
           | Comma
           | Space
           | Identifier String
           | Comment
           deriving(Show)

data NoData = NoData deriving (Show)
instance Exception NoData

exitError e = do
  putStrLn e
  exitWith $ ExitFailure 42

maybeToIO :: Maybe a -> IO a
maybeToIO Nothing = throwIO NoData
maybeToIO (Just x) = return x

main :: IO ()
main = do
  -- contents <- getContents
  -- let nonAscii = any (not . isAscii) contents
  -- when (nonAscii) (exitError "Invalid non-ascii characters")
  -- tokenized <- maybeToIO apply $ token $ ", ,"
  (tokens, s) <- maybeToIO test
  -- t <- head tokens
  print tokens

test = runParser token "/*a*/ok,,ok,hi"

token :: Parser [Token]
token = many (ignored <|> joosToken)

joosToken = comma <|> identifier

ignored = space <|> comment

comma :: Parser Token
comma = do
  string ","
  return Comma

space :: Parser Token
space = do
  some (satisfy isSpace)
  return Space

isJoosLetter :: Char -> Bool
isJoosLetter x = isLetter x || x == '_'

isJoosAlphaNum :: Char -> Bool
isJoosAlphaNum x = isJoosLetter x || isDigit x

identifier :: Parser Token
identifier = do
  c <- satisfy isJoosLetter
  cs <- many $ satisfy isJoosAlphaNum
  return $ Identifier (c:cs)

comment :: Parser Token
comment = singleLineComment <|> multiLineComment

multiLineComment :: Parser Token
multiLineComment = do
  string "/*"
  notString "*/"
  string "*/"
  return Comment

singleLineComment :: Parser Token
singleLineComment = do
  string "//"
  notString "j"
  string "//"
  return Comment
