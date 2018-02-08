import Data.Char(isAscii, isDigit, isLetter, isSpace)
import System.Exit
import Control.Monad
import Parsing
import TokenTypes
import Control.Applicative
import Control.Exception
import Data.Maybe

asLexeme :: Token -> Maybe String

asLexeme (CharLiteral _)     = Just "CharacterLiteral"
asLexeme (StringLiteral _)   = Just "StringLiteral"
asLexeme (IntLiteral _)      = Just "IntLiteral"

asLexeme Assign              = Just "="
asLexeme Add                 = Just "+"
asLexeme Subtract            = Just "-"
asLexeme Multiply            = Just "*"
asLexeme Divide              = Just "/"
asLexeme Modulus             = Just "%"
asLexeme Negate              = Just "!"

asLexeme Greater             = Just ">"
asLexeme Less                = Just "<"
asLexeme GreaterEqual        = Just ">="
asLexeme Equal               = Just "=="
asLexeme LessEqual           = Just "<="
asLexeme Inequal             = Just "!="
asLexeme BitwiseAnd          = Just "&"
asLexeme BitwiseOr           = Just "|"
asLexeme LogicalAnd          = Just "&&"
asLexeme LogicalOr           = Just "||"

asLexeme Period              = Just "."
asLexeme Comma               = Just ","
asLexeme Semicolon           = Just ";"
asLexeme LeftBrace           = Just "{"
asLexeme RightBrace          = Just "}"
asLexeme LeftParen           = Just "("
asLexeme RightParen          = Just ")"
asLexeme LeftSquare          = Just "["
asLexeme RightSquare         = Just "]"
asLexeme (InvalidOperator _) = Nothing
asLexeme Space               = Nothing
asLexeme Comment             = Nothing

asLexeme (Identifier x)
  | x `elem` ["true", "false"] = Just "BooleanLiteral"
  | x `elem` keywords          = Just x
  | otherwise                  = Just "Identifier"


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
  -- print tokens

test = runParser token "9 i //\nok,,ok,hi"

token :: Parser [Token]
token = many (whitespace <|> joosToken)

joosToken = literal <|> separator <|> operator <|> identifier
