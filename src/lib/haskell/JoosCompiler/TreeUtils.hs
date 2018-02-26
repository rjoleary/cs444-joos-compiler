module JoosCompiler.TreeUtils where

import           Data.Tree

getLeaf :: Tree a -> a
getLeaf (Node label children)
  | length children == 0 = label
  | length children > 1 = error "Tree does not have one child"
  | otherwise = getLeaf $ head children
