module TokenTypes where

data Recognizer = Recognizer String String deriving (Show)

tokenTypes = [
             -- command
             , Recognizer "commandstar" ""
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

               -- identifier
             , Recognizer "identifier" "[a-zA-Z_][0-9a-zA-Z_]*"


               -- whitespace
             , Recognizer "whitespace" "[ \t\n]*"
             ]

keywords = ["abstract", "boolean", "break", "byte","case", "catch", "char", "class", "const", "continue", "default", "do", "else", "extends", "final", "finally", "for","goto", "if", "implements","import","instanceof", "int", "interface", "native", "new", "package","private","protected","public", "return", "short", "static", "strictfp","super","switch", "synchronized", "this", "throw", "transient", "try", "void","volatile", "while"]

invalidOperators = []
