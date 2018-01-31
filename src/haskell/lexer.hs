import Data.Char

data Token = Token String String

main :: IO ()
main = do
  -- Something is broken yere
  contents <- throwIfNotAscii getContents
  let tokens = tokenize contents
  mapM_ putTokens tokens

tokenize :: String -> [Token]
tokenize x = map (const Token "id" "lexeme") (splitWhen isSpace x)

putTokens :: Token -> IO ()
putTokens (Token id lexeme) = putStrLn (id ++ " " ++ lexeme)

-- This needs to be implemented
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen pred x = [x]

-- This needs to be implemented
throwIfNotAscii x = x
