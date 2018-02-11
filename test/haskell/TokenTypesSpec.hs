module TokenTypesSpec where

import Test.Hspec
import Parsing
import TokenTypes

runJoosParser :: String -> IO ([Token], String)
runJoosParser x = maybeToIO $ runParser token x

spec :: Spec
spec = do
  describe "joosToken" $ do
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
