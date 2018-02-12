import           Control.Applicative
import           Control.Monad
import           Data.Char           (isAscii, isDigit, isLetter, isSpace)
import           Data.Maybe
import           Parsing
import           System.Exit
import           System.IO
import           TokenTypes

asLexeme :: Token -> Maybe String
asLexeme (CharLiteral _) = Just "Identifier"
asLexeme (StringLiteral _) = Just "Identifier"
asLexeme (IntLiteral _) = Just "Identifier"
asLexeme Assign = Just "="
asLexeme Add = Just "+"
asLexeme Subtract = Just "-"
asLexeme Multiply = Just "*"
asLexeme Divide = Just "/"
asLexeme Modulus = Just "%"
asLexeme Negate = Just "!"
asLexeme Greater = Just ">"
asLexeme Less = Just "<"
asLexeme GreaterEqual = Just ">="
asLexeme Equal = Just "=="
asLexeme LessEqual = Just "<="
asLexeme Inequal = Just "!="
asLexeme BitwiseAnd = Just "&"
asLexeme BitwiseOr = Just "|"
asLexeme LogicalAnd = Just "&&"
asLexeme LogicalOr = Just "||"
asLexeme Period = Just "."
asLexeme Comma = Just ","
asLexeme Semicolon = Just ";"
asLexeme LeftBrace = Just "{"
asLexeme RightBrace = Just "}"
asLexeme LeftParen = Just "("
asLexeme RightParen = Just ")"
asLexeme LeftSquare = Just "["
asLexeme RightSquare = Just "]"
asLexeme InvalidOperator = Just "DAMMIT"
asLexeme Space = Nothing
asLexeme Comment = Nothing
asLexeme (Identifier x)
  | x `elem` keywords = Just x
  | otherwise = Just "Identifier"

exitError e = do
  hPutStrLn stderr e
  exitWith $ ExitFailure 42

-- TODO: also need start and end index
main :: IO ()
main = do
  contents <- readFile "test/joos_input.txt"
  let nonAscii = any (not . isAscii) contents
  when (nonAscii) (exitError "Invalid non-ascii characters")
  (tokens, s) <- maybeToIO (runParser token contents)
  when (hasTwoConsecutiveInts tokens) $ exitError "Invalid IntLiteral"
  if (s /= [] || any ((==) InvalidOperator) tokens)
    then exitError "Could not scan"
    else putStr . unlines . map (++ " 0 0") . catMaybes . map asLexeme $ tokens

hasTwoConsecutiveInts :: [Token] -> Bool
hasTwoConsecutiveInts l
  | (length l) > 1 = if (bothIntLiteral (head l) (l !! 1))
    then True
    else hasTwoConsecutiveInts $ tail l
  | otherwise = False

bothIntLiteral :: Token -> Token -> Bool
bothIntLiteral (IntLiteral _) (IntLiteral _) = True
bothIntLiteral _ _ = False
