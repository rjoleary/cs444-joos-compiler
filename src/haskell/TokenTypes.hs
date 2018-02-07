module TokenTypes where

data Recognizer = Recognizer String String deriving (Show)

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
  -- literal
isJoosZero :: Char -> Bool
isJoosZero x == '0'

isJoosInt :: Parser Token
isJoosInt = do
  c <- satisfy isJoosZero
  cs <- many $ satisfy isDigit
  return $ Integer (c:cs)

charParser :: Parser Token
charParser = do
  string "'"
  c <- joosChar
  string "'"
  return $ CharLiteral "'" ++ c ++ "'"

stringLiteral :: Parser Token
stringLiteral = do
  char '"'
  s <- many joosChar
  char '"'
  return $ StringLiteral s



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
  return Addition

subtract :: Parser Token
subtract = do
  string "-"
  return Subtract

multipies :: Parser Token
multipies = do
  string "*"
  return Multipies

divides :: Parser Token
divides = do
  string "/"
  return Divides

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

euqal :: Parser Token
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
  return InEqual

bitwiseAnd :: Parser Token
bitwiseAnd = do
  string "&"
  return BitwiseAnd

bitwiseOr :: Parser Token
bitwiseOr = do
  string "|"
  return BitwiseOr

negate :: Parser Token
negate = do
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
  return Assignment


  -- seperator
seperator :: Parser Token
seperator = comma <|> period <|> semicolon <|> leftBrace <|> rightBrace <|> leftParent <|> rightParent <|> leftSquare <|> rightSquare

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

leftParent :: Parser Token
leftParent = do
  string "("
  return LeftParent

rightParent :: Parser Token
rightParent = do
  string ")"
  return ReftParent

leftSquare :: Parser Token
leftParent = do
  string "["
  return LeftSqaure

rightSquare :: Parser Token
rightSquare = do
  string "]"
  return RightSquare





keywords = ["abstract", "boolean", "break", "byte","case", "catch", "char",
            "class", "const", "continue", "default", "do", "else", "extends", "final",
            "finally", "for","goto", "if", "implements","import","instanceof", "int",
            "interface", "native", "new", "package","private","protected","public",
            "return", "short", "static", "strictfp","super","switch", "synchronized",
            "this", "throw", "transient", "try", "void","volatile", "while"]

invalidkeywords = ["long", "double", "float"]
invalidOperators = ["+=", "-=", "*=", "/=", "~", "?", ":", "++", "--", "^", "%", "<<", ">>", ">>>", "+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=",">>>="]
