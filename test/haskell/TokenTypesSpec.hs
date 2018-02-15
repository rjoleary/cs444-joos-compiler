module TokenTypesSpec where

import Test.Hspec
import Parsing
import TokenTypes

runJoosParser :: String -> IO ([Token], StringTag)
runJoosParser x = maybeToIO $ runParser token (zipWith CharTag x [0..])

type SimplifiedToken = (TokenName, String, Int, Int)

simplify :: [Token] -> [SimplifiedToken]
simplify = map simplifyToken

simplifyToken :: Token -> SimplifiedToken
simplifyToken t = (tokenName t, tokenString t, tokenStart t, tokenEnd t)

spec :: Spec
spec = do
  describe "Token" $ do
    it "parses recognized characters" $ do
      (tokens, s) <- runJoosParser "i"
      (simplify tokens) `shouldBe` [(Identifier, "i", 0, 1)]
      s `shouldBe` []

    it "does not parse unrecognized characters" $ do
      (tokens, s) <- runJoosParser "i#"
      (simplify tokens) `shouldBe` [(Identifier, "i", 0, 1)]
      s `shouldBe` [CharTag '#' 0]

    describe "literals" $ do
      describe "char" $ do
        it "parses chars without baskslash" $ do
          (tokens, _) <- runJoosParser "'c'"
          (simplify tokens) `shouldBe` [(CharLiteral, "c", 1, 2)]

        it "parses chars with backslashes" $ do
          (tokens, _) <- runJoosParser "'\\n'"
          (simplify tokens) `shouldBe` [(CharLiteral, "\\n", 1, 3)]

        it "does not parse unicode escapes in chars" $ do
          (tokens, _) <- runJoosParser "'\\u2297'"
          (simplify tokens) `shouldBe` []

        it "does not parse char literals with illegal backslash" $ do
          -- x is not a legal character after a backslash
          (tokens, _) <- runJoosParser "'\\x'"
          (simplify tokens) `shouldBe` []


      describe "string" $ do
        it "parses string literals without backslash" $ do
          (tokens, _) <- runJoosParser "\"str\""
          (simplify tokens) `shouldBe` [(StringLiteral, "str", 1, 4)]

        it "parses string literals with backslash" $ do
          (tokens, _) <- runJoosParser "\"\\n test \\b\""
          (simplify tokens) `shouldBe` [(StringLiteral, "\\n test \\b", 1, 11)]

        it "parses string literals with valid octal escapes" $ do
          (tokens, _) <- runJoosParser "\"\\377\"\"\\077\"\"\\000\""
          (simplify tokens) `shouldBe` [(StringLiteral, "\\377", 1, 5)
                            ,(StringLiteral, "\\077", 7, 11)
                            ,(StringLiteral, "\\000", 13, 17)]

        it "parses string literals with invalidoctal escapes" $ do
          (tokens, _) <- runJoosParser "\"\\477\""
          (simplify tokens) `shouldBe` []

        it "does not parse string literals with illegal backslash" $ do
          -- x is not a legal character after a backslash
          (tokens, _) <- runJoosParser "\"\\x\""
          (simplify tokens) `shouldBe` []

        it "does not parse unicode escapes in strings" $ do
          (tokens, _) <- runJoosParser "\"\\u2297\""
          (simplify tokens) `shouldBe` []

      describe "int" $ do
        it "parsers recognized integer zero" $ do
          (tokens, _) <- runJoosParser "0"
          (simplify tokens) `shouldBe` [(IntLiteral, "0", 0, 1)]

        it "parsers recognized nonzero intergers" $ do
          (tokens, _) <- runJoosParser "12"
          (simplify tokens) `shouldBe` [(IntLiteral, "12", 0, 2)]

        -- Lexer checks afterward
        -- it "does not parse string starting with 0" $ do
        --   (tokens, _) <- runJoosParser "012"
        --   (simplify tokens) `shouldBe` []

    describe "operator" $ do
      describe "assign" $ do
        it "parses recognized assign" $ do
          (tokens, _) <- runJoosParser "="
          (simplify tokens) `shouldBe` [(Assign, "=", 0, 1)]

      describe "add" $ do
        it "parses recognized add" $ do
          (tokens, _) <- runJoosParser "+"
          (simplify tokens) `shouldBe` [(Add, "+", 0, 1)]

      describe "subtract" $ do
        it "parses recognized subtrect" $ do
          (tokens, _) <- runJoosParser "-"
          (simplify tokens) `shouldBe` [(Subtract, "-", 0, 1)]

      describe "multiply" $ do
        it "parses recognized multiply" $ do
          (tokens, _) <- runJoosParser "*"
          (simplify tokens) `shouldBe` [(Multiply, "*", 0, 1)]

      describe "divide" $ do
        it "parses recognized divide" $ do
          (tokens, _) <- runJoosParser "/"
          (simplify tokens) `shouldBe` [(Divide, "/", 0, 1)]

      describe "modulus" $ do
        it "parses recognized modulus" $ do
          (tokens, _) <- runJoosParser "%"
          (simplify tokens) `shouldBe` [(Modulus, "%", 0, 1)]

      describe "greater" $ do
        it "parses recognized greater" $ do
          (tokens, _) <- runJoosParser ">"
          (simplify tokens) `shouldBe` [(Greater, ">", 0, 1)]

      describe "less" $ do
        it "parses recognized less" $ do
          (tokens, _) <- runJoosParser "<"
          (simplify tokens) `shouldBe` [(Less, "<", 0, 1)]

      describe "greaterequal" $ do
        it "parses recognized greater equal" $ do
          (tokens, _) <- runJoosParser ">="
          (simplify tokens) `shouldBe` [(GreaterEqual, ">=", 0, 2)]

      describe "equal" $ do
        it "parses recognized equal" $ do
          (tokens, _) <- runJoosParser "=="
          (simplify tokens) `shouldBe` [(Equal, "==", 0, 2)]

      describe "lessequal" $ do
        it "parses recognized lessequal" $ do
          (tokens, _) <- runJoosParser "<="
          (simplify tokens) `shouldBe` [(LessEqual, "<=", 0, 2)]

      describe "inequal" $ do
        it "parses recognized inequal" $ do
          (tokens, _) <- runJoosParser "!="
          (simplify tokens) `shouldBe` [(Inequal, "!=", 0, 2)]

      describe "bitwiseand" $ do
        it "parses recognized bitwiseand" $ do
          (tokens, _) <- runJoosParser "&"
          (simplify tokens) `shouldBe` [(BitwiseAnd, "&", 0, 1)]

      describe "bitwiseor" $ do
        it "parses recognized bitwiseor" $ do
          (tokens, _) <- runJoosParser "|"
          (simplify tokens) `shouldBe` [(BitwiseOr, "|", 0, 1)]

      describe "negate" $ do
        it "parses recognized negate" $ do
          (tokens, _) <- runJoosParser "!"
          (simplify tokens) `shouldBe` [(Negate, "!", 0, 1)]

      describe "logicaland" $ do
        it "parses recognized logicaland" $ do
          (tokens, _) <- runJoosParser "&&"
          (simplify tokens) `shouldBe` [(LogicalAnd, "&&", 0, 2)]

      describe "logicalor" $ do
        it "parsers recognized logicalor" $ do
          (tokens, _) <- runJoosParser "||"
          (simplify tokens) `shouldBe` [(LogicalOr, "||", 0, 2)]

    describe "seperator" $ do
      describe "comma" $ do
        it "parsers recognized comma" $ do
          (tokens, _) <- runJoosParser ","
          (simplify tokens) `shouldBe` [(Comma, ",", 0, 1)]
      describe "period" $ do
        it "parsers recognized period" $ do
          (tokens, _) <- runJoosParser "."
          (simplify tokens) `shouldBe` [(Period, ".", 0, 1)]

      describe "leftBrace" $ do
        it "parsers recognized leftBrace" $ do
          (tokens, _) <- runJoosParser "{"
          (simplify tokens) `shouldBe` [(LeftBrace, "{", 0, 1)]

      describe "rightBrace" $ do
        it "parsers recognized rightBrace" $ do
          (tokens, _) <- runJoosParser "}"
          (simplify tokens) `shouldBe` [(RightBrace, "}", 0, 1)]

      describe "leftParen" $ do
        it "parsers recognized leftParen" $ do
          (tokens, _) <- runJoosParser "("
          (simplify tokens) `shouldBe` [(LeftParen, "(", 0, 1)]

      describe "rightParen" $ do
        it "parsers recognized rightParen" $ do
          (tokens, _) <- runJoosParser ")"
          (simplify tokens) `shouldBe` [(RightParen, ")", 0, 1)]

      describe "leftSquare" $ do
        it "parsers recognized leftSquare" $ do
          (tokens, _) <- runJoosParser "["
          (simplify tokens) `shouldBe` [(LeftSquare, "[", 0, 1)]

      describe "rightSquare" $ do
        it "parsers recognized rightSquare" $ do
          (tokens, _) <- runJoosParser "]"
          (simplify tokens) `shouldBe` [(RightSquare, "]", 0, 1)]

    describe "whitespace" $ do
      describe "space" $ do
        it "parsers recognized space" $ do
          (tokens, _) <- runJoosParser " "
          (simplify tokens) `shouldBe` [(Space, " ", 0, 1)]

      describe "carriage return" $ do
        it "parsers recognized carriage return" $ do
          (tokens, _) <- runJoosParser "\r"
          (simplify tokens) `shouldBe` [(Space, "\r", 0, 1)]

      describe "newline" $ do
        it "parsers recognized newline" $ do
          (tokens, _) <- runJoosParser "\n"
          (simplify tokens) `shouldBe` [(Space, "\n", 0, 1)]

        it "parsers recognized tab" $ do
          (tokens, _) <- runJoosParser "\t"
          (simplify tokens) `shouldBe` [(Space, "\t", 0, 1)]

        it "parsers recognized formfeed" $ do
          (tokens, _) <- runJoosParser "\f"
          (simplify tokens) `shouldBe` [(Space, "\f", 0, 1)]

        it "parsers recognized the format that return followed by newline" $ do
          (tokens, _) <- runJoosParser "\r\n"
          (simplify tokens) `shouldBe` [(Space, "\r\n", 0, 2)]

      describe "comment" $ do
        it "parsers recognized comment" $ do
          (tokens, _) <- runJoosParser "///*"
          (simplify tokens) `shouldBe` [(Comment, "", invalidIdx, invalidIdx)]

        it "parsers recognizede comment starts with //" $ do
          (tokens, _) <- runJoosParser "//"
          (simplify tokens) `shouldBe` [(Comment, "", invalidIdx, invalidIdx)]

    -- Lexer checks afterward
    -- describe "invalidkeyword" $ do
    --   it "parsers recognized invaild keyword" $ do
    --     (tokens, _) <- runJoosParser "long"
    --     (simplify tokens) `shouldBe` []
