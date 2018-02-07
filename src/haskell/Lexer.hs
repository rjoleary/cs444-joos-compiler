import Data.Char(isAscii, isDigit, isLetter, isSpace)
import System.Exit
import Control.Monad
import TokenTypes
import Parsing
import Control.Applicative
import Control.Exception
import Data.Maybe

data Token = CharToken Char
           | Comma
           | LeftBrace
           | RightBrace
           | Semicolon
           | Space
           | Identifier String
           | Comment
           deriving(Show)

asLexeme (CharToken _)  = Just "Identifier"
asLexeme Comma          = Just ","
asLexeme LeftBrace      = Just "{"
asLexeme RightBrace     = Just "}"
asLexeme Semicolon      = Just ";"
asLexeme Space          = Nothing
asLexeme Comment        = Nothing

asLexeme (Identifier "public")    = Just "public"
asLexeme (Identifier "protected") = Just "protected"
asLexeme (Identifier "static")    = Just "static"
asLexeme (Identifier "int")       = Just "int"
asLexeme (Identifier "class")     = Just "class"
asLexeme (Identifier x)           = Just "Identifier"


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
  -- (tokens, s) <- maybeToIO test
  -- t <- head tokens
  -- print tokens

  contents <- readFile "test/joos_input.txt"
  (tokens, _) <- maybeToIO (runParser token contents)
  -- TODO: also need start and end index
  putStr . unlines . map (++" 0 0") . catMaybes . map asLexeme $ tokens

test = runParser token "/*a*/ok,,ok,hi"

token :: Parser [Token]
token = many (ignored <|> joosToken)

joosToken = comma <|> leftBrace <|> rightBrace <|> semicolon <|> identifier

ignored = space <|> comment

comma :: Parser Token
comma = do
  string ","
  return Comma

leftBrace :: Parser Token
leftBrace = do
  string "{"
  return LeftBrace

rightBrace :: Parser Token
rightBrace = do
  string "}"
  return RightBrace

semicolon :: Parser Token
semicolon = do
  string ";"
  return Semicolon

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
  manyTill anyChar (string "*/")
  return Comment

singleLineComment :: Parser Token
singleLineComment = do
  string "//"
  notString "j"
  string "//"
  return Comment
