module TokenTypes where

import           Control.Applicative
import           Data.Char           (isDigit, isLetter, isSpace)
import           Parsing

data Token = Token { tokenName :: TokenName
                   , tokenData :: StringTag
                   }

tokenString :: Token -> String
tokenString t = map tagChar $ tokenData t

tokenStart :: Token -> Int
tokenStart (Token _ []) = invalidIdx
tokenStart (Token _ xs) = tagIdx $ head xs

tokenEnd :: Token -> Int
tokenEnd (Token _ []) = invalidIdx
tokenEnd (Token _ xs) = (tagIdx $ last xs) + 1

data TokenName
  = CharLiteral
  | StringLiteral
  | IntLiteral
  | Identifier
  | InvalidOperator
  | Assign
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | Negate
  | Greater
  | Less
  | GreaterEqual
  | Equal
  | LessEqual
  | Inequal
  | BitwiseAnd
  | BitwiseOr
  | LogicalAnd
  | LogicalOr
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
  deriving (Eq, Show)

token :: Parser [Token]
token = many (whitespace <|> joosToken)

joosToken = literal <|> separator <|> operator <|> identifier

-- literal
literal :: Parser Token
literal = intLiteral <|> charLiteral <|> stringLiteral

intLiteral :: Parser Token
intLiteral = intZero <|> intPositiveInt

intZero :: Parser Token
intZero = do
  s <- string "0"
  return (Token IntLiteral s)

intPositiveInt :: Parser Token
intPositiveInt = do
  c <- satisfy (\x -> x > '0' && x <= '9')
  cs <- many $ satisfy isDigit
  return (Token IntLiteral (c:cs))

escapeCharacters = "btnfr'\\\""
octalDigits = "01234567"
zeroToThree = "0123"

octalEscape :: Parser StringTag
octalEscape = do {c1 <- oneOfChar zeroToThree; c2 <- oneOfChar octalDigits; c3 <- oneOfChar octalDigits; return [c1,c2,c3]}
  <|> do {c1 <- oneOfChar octalDigits; c2 <- oneOfChar octalDigits; return [c1,c2]}
  <|> do {c <- oneOfChar octalDigits; return [c]}

backslashed :: Parser StringTag
backslashed = scan
  where
    scan =
      do c <- char '\\'
         s <- octalEscape
         return (c : s)
      <|> do
         c1 <- char '\\'
         c2 <- oneOfChar escapeCharacters
         return [c1, c2]
      <|> do
         c <- satisfy $ not . (=='\\')
         return [c]

charLiteral :: Parser Token
charLiteral = do
  char '\''
  s <- backslashed
  char '\''
  return (Token CharLiteral s)

stringLiteral :: Parser Token
stringLiteral = do
  char '"'
  s <- manyTill1 (backslashed) (string "\"")
  return (Token StringLiteral s)

-- identifier
isJoosLetter :: Char -> Bool
isJoosLetter x = isLetter x || x == '_'

isJoosAlphaNum :: Char -> Bool
isJoosAlphaNum x = isJoosLetter x || isDigit x

identifier :: Parser Token
identifier = do
  c <- satisfy isJoosLetter
  cs <- many $ satisfy isJoosAlphaNum
  return (Token Identifier (c:cs))

-- operator
addition :: Parser Token
addition = do
  s <- string "+"
  return (Token Add s)

subtraction :: Parser Token
subtraction = do
  s <- string "-"
  return (Token Subtract s)

multiplies :: Parser Token
multiplies = do
  s <- string "*"
  return (Token Multiply s)

divides :: Parser Token
divides = do
  s <- string "/"
  return (Token Divide s)

modulus :: Parser Token
modulus = do
  s <- string "%"
  return (Token Modulus s)

greater :: Parser Token
greater = do
  s <- string ">"
  return (Token Greater s)

less :: Parser Token
less = do
  s <- string "<"
  return (Token Less s)

greaterEqual :: Parser Token
greaterEqual = do
  s <- string ">="
  return (Token GreaterEqual s)

equal :: Parser Token
equal = do
  s <- string "=="
  return (Token Equal s)

lessEqual :: Parser Token
lessEqual = do
  s <- string "<="
  return (Token LessEqual s)

inequal :: Parser Token
inequal = do
  s <- string "!="
  return (Token Inequal s)

bitwiseAnd :: Parser Token
bitwiseAnd = do
  s <- string "&"
  return (Token BitwiseAnd s)

bitwiseOr :: Parser Token
bitwiseOr = do
  s <- string "|"
  return (Token BitwiseOr s)

negation :: Parser Token
negation = do
  s <- string "!"
  return (Token Negate s)

logicalAnd :: Parser Token
logicalAnd = do
  s <- string "&&"
  return (Token LogicalAnd s)

logicalOr :: Parser Token
logicalOr = do
  s <- string "||"
  return (Token LogicalOr s)

assignment :: Parser Token
assignment = do
  s <- string "="
  return (Token Assign s)

operator = invalidOperator <|> doubleCharOperator <|> singleCharOperator

doubleCharOperator = lessEqual <|> greaterEqual <|> equal <|> inequal

singleCharOperator =
  logicalAnd <|> logicalOr <|> addition <|> subtraction <|> multiplies <|>
  divides <|>
  modulus <|>
  greater <|>
  less <|>
  bitwiseAnd <|>
  bitwiseOr <|>
  negation <|>
  assignment

-- separator
separator :: Parser Token
separator =
  comma <|> period <|> semicolon <|> leftBrace <|> rightBrace <|> leftParen <|>
  rightParen <|>
  leftSquare <|>
  rightSquare

comma :: Parser Token
comma = do
  s <- string ","
  return (Token Comma s)

period :: Parser Token
period = do
  s <- string "."
  return (Token Period s)

semicolon :: Parser Token
semicolon = do
  s <- string ";"
  return (Token Semicolon s)

leftBrace :: Parser Token
leftBrace = do
  s <- string "{"
  return (Token LeftBrace s)

rightBrace :: Parser Token
rightBrace = do
  s <- string "}"
  return (Token RightBrace s)

leftParen :: Parser Token
leftParen = do
  s <- string "("
  return (Token LeftParen s)

rightParen :: Parser Token
rightParen = do
  s <- string ")"
  return (Token RightParen s)

leftSquare :: Parser Token
leftSquare = do
  s <- string "["
  return (Token LeftSquare s)

rightSquare :: Parser Token
rightSquare = do
  s <- string "]"
  return (Token RightSquare s)

-- Whitespace
whitespace = space <|> comment

space :: Parser Token
space = do
  s <- some (satisfy isSpace)
  return (Token Space s)

comment :: Parser Token
comment = singleLineComment <|> multiLineComment

multiLineComment :: Parser Token
multiLineComment = do
  string "/*"
  manyTill anyChar (string "*/")
  return (Token Comment [])

singleLineComment :: Parser Token
singleLineComment = do
  string "//"
  many $ satisfy $ not . (==) '\n'
  return (Token Comment [])

-- TODO
-- invalidOperator
invalidOperator :: Parser Token
invalidOperator = (foldr (<|>) empty invalidOperators)

invalidOperators :: [(Parser Token)]
invalidOperators =
  map
    (\x -> do
       s <- string x
       return $ (Token InvalidOperator s))
    invalidOperatorList

keywords =
  [ "abstract"
  , "boolean"
  , "byte"
  , "char"
  , "class"
  , "else"
  , "extends"
  , "final"
  , "for"
  , "if"
  , "implements"
  , "import"
  , "instanceof"
  , "int"
  , "interface"
  , "native"
  , "new"
  , "package"
  , "protected"
  , "public"
  , "return"
  , "short"
  , "static"
  , "super"
  , "this"
  , "void"
  , "while"
  ]

invalidkeywords =
  [ "break"
  , "case"
  , "catch"
  , "const"
  , "continue"
  , "default"
  , "do"
  , "double"
  , "finally"
  , "float"
  , "goto"
  , "long"
  , "private"
  , "strictfp"
  , "switch"
  , "synchronized"
  , "throw"
  , "throws"
  , "transient"
  , "try"
  , "volatile"
  ]

invalidOperatorList =
  [ "+="
  , "-="
  , "*="
  , "/="
  , "~"
  , "?"
  , ":"
  , "++"
  , "--"
  , "^"
  , ">>>"
  , "<<"
  , ">>"
  , "+="
  , "-="
  , "*="
  , "/="
  , "&="
  , "|="
  , "^="
  , "%="
  , "<<="
  , ">>="
  , ">>>="
  ]
