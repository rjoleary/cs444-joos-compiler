module JoosCompiler.TreeUtils where

import           Data.Data
import           Data.Tree

getLeaf :: Tree a -> a
getLeaf (Node label children)
  | length children == 0 = label
  | length children > 1 = error "Tree does not have one child"
  | otherwise = getLeaf $ head children

findChildren :: (a -> Bool) -> Tree a -> [Tree a]
findChildren predicate t@(Node label children)
  | predicate label = t : (mconcat $ map (findChildren predicate) children)
  | otherwise = mconcat $ map (findChildren predicate) children

findChild :: (a -> Bool) -> Tree a -> Tree a
findChild predicate t
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = findChildren predicate t
