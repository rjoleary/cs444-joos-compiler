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

findDirectChildren :: (a -> Bool) -> (a -> Bool) -> Tree a -> [Tree a]
findDirectChildren childPred indirectPred t@(Node label children)
  | childPred label = t : childMatches
  | indirectPred label = []
  | otherwise = childMatches
  where
    childMatches = findDirectChildren1 childPred indirectPred children

findDirectChild :: (a -> Bool) -> (a -> Bool) -> Tree a -> Tree a
findDirectChild childPred indirectPred t
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = findDirectChildren childPred indirectPred t

findDirectChildren1 :: (a -> Bool) -> (a -> Bool) -> [Tree a] -> [Tree a]
findDirectChildren1 childPred indirectPred ts =
  mconcat $ map (findDirectChildren childPred indirectPred) ts

findDirectChild1 :: (a -> Bool) -> (a -> Bool) -> [Tree a] -> Tree a
findDirectChild1 childPred indirectPred ts
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = findDirectChildren1 childPred indirectPred ts
