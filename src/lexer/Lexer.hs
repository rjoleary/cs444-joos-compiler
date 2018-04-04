import           Control.Applicative
import           Control.Monad
import           System.Environment
import           Data.Char           (isAscii, isDigit, isLetter, isSpace)
import           Data.Maybe
import           JoosCompiler.Exit
import           Parsing
import           TokenTypes

maxNegativeInt = 2147483648

asLexeme :: Token -> Maybe String
asLexeme (Token CharLiteral _) = Just "CharacterLiteral"
asLexeme (Token StringLiteral _) = Just "StringLiteral''''"
asLexeme (Token IntLiteral _) = Just "IntegerLiteral"
asLexeme (Token Assign _) = Just "="
asLexeme (Token Add _) = Just "+"
asLexeme (Token Subtract _) = Just "-"
asLexeme (Token Multiply _) = Just "*"
asLexeme (Token Divide _) = Just "/"
asLexeme (Token Modulus _) = Just "%"
asLexeme (Token Negate _) = Just "!"
asLexeme (Token Greater _) = Just ">"
asLexeme (Token Less _) = Just "<"
asLexeme (Token GreaterEqual _) = Just ">="
asLexeme (Token Equal _) = Just "=="
asLexeme (Token LessEqual _) = Just "<="
asLexeme (Token Inequal _) = Just "!="
asLexeme (Token BitwiseAnd _) = Just "&"
asLexeme (Token BitwiseOr _) = Just "|"
asLexeme (Token LogicalAnd _) = Just "&&"
asLexeme (Token LogicalOr _) = Just "||"
asLexeme (Token Period _) = Just "."
asLexeme (Token Comma _) = Just ","
asLexeme (Token Semicolon _) = Just ";"
asLexeme (Token LeftBrace _) = Just "{"
asLexeme (Token RightBrace _) = Just "}"
asLexeme (Token LeftParen _) = Just "("
asLexeme (Token RightParen _) = Just ")"
asLexeme (Token LeftSquare _) = Just "["
asLexeme (Token RightSquare _) = Just "]"
asLexeme (Token InvalidOperator _) = Just "INVALID"
asLexeme (Token Space _) = Nothing
asLexeme (Token Comment _) = Nothing
asLexeme t@(Token Identifier _)
  | (tokenString t) `elem` keywords = Just (tokenString t)
  | (tokenString t) `elem` ["true", "false"] = Just "BooleanLiteral"
  | (tokenString t) == "null" = Just "NullLiteral"
  | otherwise = Just "Identifier"

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  let nonAscii = any (not . isAscii) contents
  when (nonAscii) (exitError "Invalid non-ascii characters")
  (tokens, s) <- maybeToIO (runParser token (zipWith CharTag contents [0 ..]))
  when (hasTwoConsecutiveInts tokens) $ exitError "Invalid IntLiteral"
  when (hasInvalidKeywords tokens) $ exitError "Invalid keywords"
  when (intOutsideRange tokens) $ exitError "Integer outside range"
  if (s /= [] || any ((==) InvalidOperator) (map tokenName tokens))
    then exitError "Could not scan"
    else putStr . unlines . catMaybes . map formatTokenLine $ tokens

formatTokenLine :: Token -> Maybe String
formatTokenLine t = formatLine (asLexeme t)
  where
    formatLine (Nothing) = Nothing
    formatLine (Just terminal) =
      Just (terminal ++ " " ++ show (tokenStart t) ++ " " ++ show (tokenEnd t))

hasTwoConsecutiveInts :: [Token] -> Bool
hasTwoConsecutiveInts l
  | (length l) > 1 =
    if (bothIntLiteral (head l) (l !! 1))
      then True
      else hasTwoConsecutiveInts $ tail l
  | otherwise = False

bothIntLiteral :: Token -> Token -> Bool
bothIntLiteral (Token IntLiteral _) (Token IntLiteral _) = True
bothIntLiteral _ _                                       = False

hasInvalidKeywords :: [Token] -> Bool
hasInvalidKeywords l
  | (length l) > 0 =
    if (isInvalidKeywords (head l))
      then True
      else hasInvalidKeywords $ tail l
  | otherwise = False

isInvalidKeywords :: Token -> Bool
isInvalidKeywords t@(Token Identifier _)
  | (tokenString t) `elem` invalidkeywords = True
  | otherwise = False
isInvalidKeywords _ = False

intOutsideRange :: [Token] -> Bool
intOutsideRange l
  | any ((> maxNegativeInt) . read . tokenString) $ (filter isIntLiteral l) =
    True
  | otherwise = False
  where
    isIntLiteral t = tokenName t == IntLiteral
