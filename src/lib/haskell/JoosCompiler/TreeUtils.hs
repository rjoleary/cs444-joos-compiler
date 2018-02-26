module JoosCompiler.TreeUtils where

import           Data.Data
import           Data.Tree

getLeaves :: Tree a -> [a]
getLeaves (Node label children)
  | length children == 0 = [label]
  | otherwise = mconcat $ map getLeaves children

getLeaf :: Tree a -> a
getLeaf (Node label children)
  | length children == 0 = label
  | length children > 1 = error "Tree does not have one child"
  | otherwise = getLeaf $ head children

findChildren :: (a -> Bool) -> Tree a -> [Tree a]
findChildren predicate t@(Node label children)
  | predicate label = t : (mconcat $ map (findChildren predicate) children)
  | otherwise = mconcat $ map (findChildren predicate) children

findChildren1 :: (a -> Bool) -> [Tree a] -> [Tree a]
findChildren1 predicate ts = mconcat $ map (findChildren predicate) ts

findChild :: (a -> Bool) -> Tree a -> Tree a
findChild predicate t
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = findChildren predicate t

findChild1 :: (a -> Bool) -> [Tree a] -> Tree a
findChild1 predicate t
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = mconcat $ map (findChildren predicate) t

findDirectChildren :: (a -> Bool) -> (a -> Bool) -> (Tree a) -> [Tree a]
findDirectChildren childPred indirectPred t@(Node label children)
  | childPred label = [t]
  | indirectPred label = []
  | otherwise =
    mconcat $ map (findDirectChildren childPred indirectPred) children
