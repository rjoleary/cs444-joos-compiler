module JoosCompiler.Treeify where

import           Data.List
import           Data.Maybe
import           Data.Tree

type UntaggedToken = String
type UntaggedParseTree = Tree UntaggedToken

-- Parse a single production rule into a tree with a single parent node.
parseProduction :: String -> UntaggedParseTree
parseProduction line = Node rhs (map singleNode lhs)
  where
    rhs:lhs = words line

-- Convert the bottom-up parse to a Haskell datatype.
treeify :: String -> UntaggedParseTree
treeify x =
  case foldl treeify' [] (map parseProduction . lines $ x) of
    [t] -> t
    _   -> error "Could not create tree"

-- Run this for each rule added to the tree.
treeify' :: [UntaggedParseTree] -> UntaggedParseTree -> [UntaggedParseTree]
treeify' forest rule = rule' : forest'
  where
    lhsEq x y = lhs x == lhs y
        -- TODO: The following line fails on rules with multiple of the same
        -- token on the RHS. It is not an issue for our current grammar.
    rule' =
      Node (lhs rule) [fromMaybe x $ find (lhsEq x) forest | x <- rhs rule]
    forest' = deleteFirstsBy lhsEq forest (rhs rule)

singleNode :: a -> Tree a
singleNode x = Node x []

lhs :: Tree t -> t
lhs (Node x _) = x

rhs :: Tree t -> [Tree t]
rhs (Node _ x) = x
