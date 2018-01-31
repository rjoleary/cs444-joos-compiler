import Data.Char

data Token = Token String String

main :: IO ()
main = do
  contents <- getContents
  let tokens = tokenize contents
  map putTokens tokens

tokenize :: String -> [Token]
tokenize x = splitWhen isSpace x

putTokens :: Token -> IO ()
putTokens (Token id lexeme) = putStrLn x

-- This needs to be implemented
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen pred (x:xs)
