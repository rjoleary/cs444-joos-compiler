import           Data.List
import           Data.Maybe
import           Data.Tree

type ParseTree = Tree String

singleNode = flip Node []

lhs (Node x _) = x

rhs (Node _ x) = x

-- Reverse map http://snipplr.com/view/18351/reverse-map/
pam lf x = map g lf where
  g f = f x

nand x y = (not x) || (not y)
nor x y = (not x) && (not y)

classDeclaration = "ClassDeclaration"
abstract = "abstract"
final = "final"

-- Parse a single production rule into a tree with a single parent node.
parseProduction :: String -> ParseTree
parseProduction line = Node rhs (map singleNode lhs)
  where
    rhs:lhs = words line

-- Convert the bottom-up parse to a Haskell datatype.
--treeify :: String -> ParseTree
treeify x =
  case foldl treeify' [] (map parseProduction . lines $ x) of
    [t] -> t
    _   -> error "Could not create tree"

-- Run this for each rule added to the tree.
treeify' :: [ParseTree] -> ParseTree -> [ParseTree]
treeify' forest rule = rule' : forest'
  where
    lhsEq x y = lhs x == lhs y
        -- TODO: The following line fails on rules with multiple of the same
        -- token on the RHS. It is not an issue for our current grammar.
    rule' =
      Node (lhs rule) [fromMaybe x $ find (lhsEq x) forest | x <- rhs rule]
    forest' = deleteFirstsBy lhsEq forest (rhs rule)

hasModifier :: String -> ParseTree -> Bool
hasModifier modifier tree
  | rootLabel tree == modifier = True
  | otherwise = any (hasModifier modifier) $ subForest tree

notAbstractFinal :: ParseTree -> Bool
notAbstractFinal tree =
  if (rootLabel tree) == classDeclaration
  then (hasModifier abstract tree) && (hasModifier final tree)
  else any id $ map notAbstractFinal $ subForest tree

bodyIfNotAbstractOrNative tree =
  True

abstractMethodNotStaticOrFinal tree =
  True

staticMethodNotFinal tree =
  True

nativeMethodStatic tree =
  True

voidTypeOnlyUsedAsMethodReturn tree =
  True

classnameSameAsFilename tree =
  True

interfaceMethodNotStaticFinalOrNative tree =
  True

classAtLeastOneConstructor tree =
  True

noFinalField tree =
  True

integerWithinRange tree =
  True

weederRules = [notAbstractFinal]

weed tree =
  any id (pam weederRules tree)

main = do
  contents <- readFile "test/joos_tree.txt"
  let tree = treeify contents
  print $ weed tree
    --putStrLn $ case weed tree of
    --    Just err -> "Error: " ++ err
    --    Nothing  -> "OK"
