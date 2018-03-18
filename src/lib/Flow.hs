module Flow where

-- Inspired by Flow
-- http://taylor.fausak.me/2015/04/09/write-more-understandable-haskell-with-flow/

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 0 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f1 .> f2 = f2 . f1
