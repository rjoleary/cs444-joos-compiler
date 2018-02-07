module TokenTypes where

import Control.Applicative
import Data.Char(isAscii, isDigit, isLetter, isSpace)
import Parsing

data Recognizer = Recognizer String String deriving (Show)

data Token = CharLiteral String
           | StringLiteral String
           | IntLiteral String
           | Identifier String
           | Add
           | Subtract
           | Multiply
           | Divide
           | Greater
           | Less
           | GreaterEqual
           | Equal
           | LessEqual
           | Inequal
           | BitwiseAnd
           | BitwiseOr
           | Negate
           | LogicalAnd
           | LogicalOr
           | Assign
           | Period
           | Comma
           | Semicolon
           | LeftBrace
           | RightBrace
           | LeftParen
           | RightParen
           | LeftSquare
           | RightSquare
           | Space
           | Comment
           deriving(Show)

-- literal

literal :: Parser Token
literal = intLiteral <|> charLiteral <|> stringLiteral

intLiteral :: Parser Token
intLiteral = intZero <|> intPositiveInt

intZero :: Parser Token
intZero = do
  s <- string "0"
  return $ IntLiteral s

intPositiveInt :: Parser Token
intPositiveInt = do
  c <- satisfy (\x -> x > '0' && x <= '9')
  cs <- many $ satisfy isDigit
  return $ IntLiteral $ c:cs

escapeCharacters = "btnfr01234567'\\\""

backslashed :: Parser Char -> [Char] -> Parser String
backslashed c e = scan where
  scan = do {char '\\'; c <- oneOfChar e; return $ "\\" ++ e} <|> do {c <- anyChar; return [c]}


charLiteral :: Parser Token
charLiteral = do
  string "'"
  s <- backslashed anyChar escapeCharacters
  string "'"
  return $ CharLiteral s

stringLiteral :: Parser Token
stringLiteral = do
  char '"'
  ss <- many $ backslashed anyChar escapeCharacters
  char '"'
  return $ StringLiteral $ mconcat ss

-- identifier

isJoosLetter :: Char -> Bool
isJoosLetter x = isLetter x || x == '_'

isJoosAlphaNum :: Char -> Bool
isJoosAlphaNum x = isJoosLetter x || isDigit x

identifier :: Parser Token
identifier = do
  c <- satisfy isJoosLetter
  cs <- many $ satisfy isJoosAlphaNum
  return $ Identifier (c:cs)

-- operator

addition :: Parser Token
addition = do
  string "+"
  return Add

subtraction :: Parser Token
subtraction = do
  string "-"
  return Subtract

multiplies :: Parser Token
multiplies = do
  string "*"
  return Multiply

divides :: Parser Token
divides = do
  string "/"
  return Divide

greater :: Parser Token
greater = do
  string ">"
  return Greater

less :: Parser Token
less = do
  string "<"
  return Less

greaterEqual :: Parser Token
greaterEqual = do
  string ">="
  return GreaterEqual

equal :: Parser Token
equal = do
  string "=="
  return Equal


lessEqual :: Parser Token
lessEqual = do
  string "<="
  return LessEqual

inequal :: Parser Token
inequal = do
  string "!="
  return Inequal

bitwiseAnd :: Parser Token
bitwiseAnd = do
  string "&"
  return BitwiseAnd

bitwiseOr :: Parser Token
bitwiseOr = do
  string "|"
  return BitwiseOr

negation :: Parser Token
negation = do
  string "!"
  return Negate

logicalAnd :: Parser Token
logicalAnd = do
  string "&&"
  return LogicalAnd

logicalOr :: Parser Token
logicalOr = do
  string "||"
  return LogicalOr

assignment :: Parser Token
assignment = do
  string "="
  return Assign

operator = addition <|> subtraction <|> multiplies <|> divides <|> greater <|>
  less <|> greaterEqual <|> equal <|> lessEqual <|> inequal <|> bitwiseAnd <|>
  bitwiseOr <|> negation <|> logicalAnd <|> logicalOr <|> assignment

-- separator

separator :: Parser Token
separator = comma <|> period <|> semicolon <|> leftBrace <|> rightBrace <|> leftParen <|> rightParen <|> leftSquare <|> rightSquare

comma :: Parser Token
comma = do
  string ","
  return Comma

period :: Parser Token
period = do
  string "."
  return Period

semicolon :: Parser Token
semicolon = do
  string ";"
  return Semicolon

leftBrace :: Parser Token
leftBrace = do
  string "{"
  return LeftBrace

rightBrace :: Parser Token
rightBrace = do
  string "}"
  return RightBrace

leftParen :: Parser Token
leftParen = do
  string "("
  return LeftParen

rightParen :: Parser Token
rightParen = do
  string ")"
  return RightParen

leftSquare :: Parser Token
leftSquare = do
  string "["
  return LeftSquare

rightSquare :: Parser Token
rightSquare = do
  string "]"
  return RightSquare

-- Whitespace

whitespace = space <|> comment

space :: Parser Token
space = do
  some (satisfy isSpace)
  return Space

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
  many $ satisfy $ not . (==) '\n'
  return Comment




keywords = ["abstract", "boolean", "break", "byte","case", "catch", "char",
            "class", "const", "continue", "default", "do", "else", "extends", "final",
            "finally", "for","goto", "if", "implements","import","instanceof", "int",
            "interface", "native", "new", "package","private","protected","public",
            "return", "short", "static", "strictfp","super","switch", "synchronized",
            "this", "throw", "transient", "try", "void","volatile", "while"]

invalidkeywords = ["long", "double", "float"]
invalidOperators = ["+=", "-=", "*=", "/=", "~", "?", ":", "++", "--",
                    "^", "%", "<<", ">>", ">>>", "+=","-=","*=","/=",
                    "&=", "|=","^=","%=","<<=",">>=",">>>="]

-- Obsolete. Will delete once we don't need it any longer

             -- comments
tokenTypes = [ Recognizer "commandstar" ""
             , Recognizer "commandslash" ""
             , Recognizer "javadoccommand" ""

             -- literal
             , Recognizer "zeroliteral" "0"
             , Recognizer "pintliteral" "[1-9][0-9]*"
             , Recognizer "strliteral" ""
             , Recognizer "nullliteral" "[]"
             , Recognizer "boolliteral" "[]"
             , Recognizer "charliteral" "'.*'"
             , Recognizer "characterescapes" "[]"

               -- seperator
             , Recognizer "comma" ","
             , Recognizer "period" "."
             , Recognizer "semicolon" ";"
             , Recognizer "lparenthese" "("
             , Recognizer "rparenthese" ")"
             , Recognizer "lcurlybracket" "{"
             , Recognizer "rcurlybracket" "}"
             , Recognizer "lsquarebracket" "["
             , Recognizer "rsquarebracket" "]"

               -- operator
             , Recognizer "addition" "+"
             , Recognizer "subtract" "-"
             , Recognizer "multiplies" "*"
             , Recognizer "divides" "/"
             , Recognizer "greater" ">"
             , Recognizer "less" "<"
             , Recognizer "greater_equal" ">="
             , Recognizer "less_equal" "<="
             , Recognizer "equal" "=="
             , Recognizer "inequal" "!="
             , Recognizer "bitwise_and" "&"
             , Recognizer "bitwise_or" "|"
             , Recognizer "negate" "!"
             , Recognizer "logical_and" "&&"
             , Recognizer "logical_or" "||"
             , Recognizer "assignment" "="

               -- identifier
             , Recognizer "identifier" "[a-zA-Z_][0-9a-zA-Z_]*"


               -- whitespace
             , Recognizer "whitespace" "[ \t\n]*"
             ]
