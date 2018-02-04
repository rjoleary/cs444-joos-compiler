data Recognizer = Recognizer String String deriving (Show)

tokenTypes = [
  -- command
  Recognizer "commandstar" "",
  Recognizer "commandslash" "",
  Recognizer "javadoccommand" "",

  -- literal
  Recognizer "intliteral" "",
  Recognizer "boolliteral" "[]",
  Recognizer "strliteral" "[]",
  Recognizer "nullliteral" "[]",
  Recognizer "charliteral" "[]",
  Recognizer "compoundnames" "[]",
  Recognizer "thisprimaryexpression" "[]",
  Recognizer "characterescapes" "[]",

  -- seperator
  Recognizer "comma" ",",
  Recognizer "semicolon" ";",
  
  -- operator
  Recognizer "equals" "=",

  -- identifier
  Recognizer "identifier" "[a-zA-Z_][0-9a-zA-Z_]*",


  -- whitespace
  Recognizer "space" "",
  Recognizer "newline" "",
    Recognizer "tab" ""
  ]

keywords = []


invalidOperators = []
