module TokenTypesSpec where

import Test.Hspec
import Parsing
import TokenTypes

runJoosParser :: String -> IO ([Token], String)
runJoosParser x = maybeToIO $ runParser token x

spec :: Spec
spec = do
  describe "Token" $ do
    it "parses recognized characters" $ do
      (tokens, s) <- runJoosParser "i"
      tokens `shouldBe` [Identifier "i"]
      s `shouldBe` ""

    it "does not parse unrecognized characters" $ do
      (tokens, s) <- runJoosParser "i#"
      tokens `shouldBe` [Identifier "i"]
      s `shouldBe` "#"

    describe "literals" $ do
      describe "char" $ do
        it "parses chars without baskslash" $ do
          (tokens, _) <- runJoosParser "'c'"
          tokens `shouldBe` [CharLiteral "c"]

        it "parses chars with backslashes" $ do
          (tokens, _) <- runJoosParser "'\\n'"
          tokens `shouldBe` [CharLiteral "\\n"]

        it "does not parse unicode escapes in chars" $ do
          (tokens, _) <- runJoosParser "'\\u2297'"
          tokens `shouldBe` []

        it "does not parse char literals with illegal backslash" $ do
          -- x is not a legal character after a backslash
          (tokens, _) <- runJoosParser "'\\x'"
          tokens `shouldBe` []


      describe "string" $ do
        it "parses string literals without backslash" $ do
          (tokens, _) <- runJoosParser "\"str\""
          tokens `shouldBe` [StringLiteral "str"]

        it "parses string literals with backslash" $ do
          (tokens, _) <- runJoosParser "\"\\n test \\b\""
          tokens `shouldBe` [StringLiteral "\\n test \\b"]

        it "does not parse string literals with illegal backslash" $ do
          -- x is not a legal character after a backslash
          (tokens, _) <- runJoosParser "\"\\x\""
          tokens `shouldBe` []

        it "does not parse unicode escapes in strings" $ do
          (tokens, _) <- runJoosParser "\"\\u2297\""
          tokens `shouldBe` []

      describe "int" $ do
        it "parsers recognized integer zero" $ do
          (tokens, _) <- runJoosParser "0"
          tokens `shouldBe` [IntLiteral "0"]

        it "parsers recognized nonzero intergers" $ do
          (tokens, _) <- runJoosParser "12"
          tokens `shouldBe` [IntLiteral "12"]

        it "does not parse string starting with 0" $ do
          (tokens, _) <- runJoosParser "012"
          tokens `shouldBe` []
    describe "operator" $ do
      describe "assign" $ do
        it "parses recognized assign" $ do
          (tokens, _) <- runJoosParser "="
          tokens `shouldBe` [Assign]

      describe "add" $ do
        it "parses recognized add" $ do
          (tokens, _) <- runJoosParser "+"
          tokens `shouldBe` [Add]

      describe "subtract" $ do
        it "parses recognized subtrect" $ do
          (tokens, _) <- runJoosParser "-"
          tokens `shouldBe` [Subtract]

      describe "multiply" $ do
        it "parses recognized multiply" $ do
          (tokens, _) <- runJoosParser "*"
          tokens `shouldBe` [Multiply]

      describe "divide" $ do
        it "parses recognized divide" $ do
          (tokens, _) <- runJoosParser "/"
          tokens `shouldBe` [Divide]

      describe "modulus" $ do
        it "parses recognized modulus" $ do
          (tokens, _) <- runJoosParser "%"
          tokens `shouldBe` [Modulus]

      describe "greater" $ do
        it "parses recognized greater" $ do
          (tokens, _) <- runJoosParser ">"
          tokens `shouldBe` [Greater]

      describe "less" $ do
        it "parses recognized less" $ do
          (tokens, _) <- runJoosParser "<"
          tokens `shouldBe` [Less]

      describe "greaterequal" $ do
        it "parses recognized greater equal" $ do
          (tokens, _) <- runJoosParser ">="
          tokens `shouldBe` [GreaterEqual]

      describe "equal" $ do
        it "parses recognized equal" $ do
          (tokens, _) <- runJoosParser "=="
          tokens `shouldBe` [Equal]

      describe "lessequal" $ do
        it "parses recognized lessequal" $ do
          (tokens, _) <- runJoosParser "<="
          tokens `shouldBe` [LessEqaul]

      describe "inequal" $ do
        it "parses recognized inequal" $ do
          (tokens, _) <- runJoosParser "!="
          tokens `shouldBe` [Inequal]

      describe "bitwiseand" $ do
        it "parses recognized bitwiseand" $ do
          (tokens, _) <- runJoosParser "&"
          tokens `shouldBe` [BitwiseAnd]

      describe "bitwiseor" $ do
        it "parses recognized bitwiseor" $ do
          (tokens, _) <- runJoosParser "|"
          tokens `shouldBe` [BitwiseOr]

      describe "negate" $ do
        it "parses recognized negate" $ do
          (tokens, _) <- runJoosParser "!"
          tokens `shouldBe` [Negate]

      describe "logicaland" $ do
        it "parses recognized logicaland" $ do
          (tokens, _) <- runJoosParser "$$"
          tokens `shouldBe` [LogicalAnd]

      describe "logicalor" $ do
        it "parsers recognized logicalor" $ do
          (tokens, _) <- runJoosParser "||"
          tokens `shouldBe` [LogicalOr]

    describe "seperator"
      describe "comma" $ do
        it "parsers recognized comma" $ do
          (tokens, _) <- runJoosParser ","
          tokens `shouldBe` [Comma]
      describe "period" $ do
        it "parsers recognized period" $ do
          (tokens, _) <- runJoosParser "."
          tokens `shouldBe` [Period]

      describe "leftBrace" $ do
        it "parsers recognized leftBrace" $ do
          (tokens, _) <- runJoosParser "{"
          tokens `shouldBe` [LeftBrace]

      describe "rightBrace" $ do
        it "parsers recognized rightBrace" $ do
          (tokens, _) <- runJoosParser "}"
          tokens `shouldBe` [RightBrace]

      describe "leftParen" $ do
        it "parsers recognized leftParen" $ do
          (tokens, _) <- runJoosParser "("
          tokens `shouldBe` [LeftParen]

      describe "rightParen" $ do
        it "parsers recognized rightParen" $ do
          (tokens, _) <- runJoosParser ")"
          tokens `shouldBe` [RightParen]

      describe "leftSquare" $ do
        it "parsers recognized leftSquare" $ do
          (tokens, _) <- runJoosParser "["
          tokens `shouldBe` [LeftSquare]

      describe "rightSquare" $ do
        it "parsers recognized rightSquare" $ do
          (tokens, _) <- runJoosParser "]"
          tokens `shouldBe` [RightSquare]

    describe "whitespace"
      describe "space" $ do
        it "parsers recognized space" $ do
          (tokens, _) <- runJoosParser " "
          tokens `shouldBe` [Space]

      describe "comment" $ do
        it "parsers recognized comment" $ do
          (tokens, _) <- runJoosParser "/*//"
          tokens `shouldBe` [Comment]
