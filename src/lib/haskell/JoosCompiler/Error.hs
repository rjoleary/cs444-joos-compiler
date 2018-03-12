module JoosCompiler.Error where

-- Collapse all the error messages into one error message.
foldEither :: [Either a b] -> Either a [b]
foldEither []           = Right []
foldEither (Left x:xs)  = Left x
foldEither (Right x:xs) = either Left (\xs -> Right (x:xs)) (foldEither xs)
