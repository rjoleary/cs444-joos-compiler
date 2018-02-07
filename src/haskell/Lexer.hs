import Data.Char(isAscii, isDigit, isLetter, isSpace)
import System.Exit
import Control.Monad
import Parsing
import TokenTypes
import Control.Applicative
import Control.Exception
import Data.Maybe

asLexeme (CharLiteral _)  = Just "Identifier"
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
  contents <- readFile "test/joos_input.txt"

  let nonAscii = any (not . isAscii) contents
  when (nonAscii) (exitError "Invalid non-ascii characters")

  -- (tokens, s) <- maybeToIO test
  (tokens, _) <- maybeToIO (runParser token contents)

  -- TODO: also need start and end index
  putStr . unlines . map (++" 0 0") . catMaybes . map asLexeme $ tokens

test = runParser token "9 i //\nok,,ok,hi"

token :: Parser [Token]
token = many (whitespace <|> joosToken)

joosToken = literal <|> separator <|> operator <|> identifier

ignored = space <|> comment
