import           Data.List
import           Data.Maybe
import           Data.Tree

type ParseTree = Tree String

singleNode = flip Node []

lhs :: Tree t -> t
lhs (Node x _) = x

rhs :: Tree t -> [Tree t]
rhs (Node _ x) = x

-- Reverse map http://snipplr.com/view/18351/reverse-map/
pam :: [a -> b] -> a -> [b]
pam lf x = map g lf where
  g f = f x

kClassDeclaration :: String
kClassDeclaration = "ClassDeclaration"
kAbstract :: String
kAbstract = "abstract"
kFinal :: String
kFinal = "final"

-- Parse a single production rule into a tree with a single parent node.
parseProduction :: String -> ParseTree
parseProduction line = Node rhs (map singleNode lhs)
  where
    rhs:lhs = words line

-- Convert the bottom-up parse to a Haskell datatype.
treeify :: String -> ParseTree
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
  if (rootLabel tree) == kClassDeclaration
  then (hasModifier kAbstract tree) && (hasModifier kFinal tree)
  else any id $ map notAbstractFinal $ subForest tree

bodyIfNotAbstractOrNative :: ParseTree -> Bool
bodyIfNotAbstractOrNative tree =
  True

abstractMethodNotStaticOrFinal :: ParseTree -> Bool
abstractMethodNotStaticOrFinal tree =
  True

staticMethodNotFinal :: ParseTree -> Bool
staticMethodNotFinal tree =
  True

nativeMethodStatic :: ParseTree -> Bool
nativeMethodStatic tree =
  True

voidTypeOnlyUsedAsMethodReturn :: ParseTree -> Bool
voidTypeOnlyUsedAsMethodReturn tree =
  True

classnameSameAsFilename :: ParseTree -> Bool
classnameSameAsFilename tree =
  True

interfaceMethodNotStaticFinalOrNative :: ParseTree -> Bool
interfaceMethodNotStaticFinalOrNative tree =
  True

classAtLeastOneConstructor :: ParseTree -> Bool
classAtLeastOneConstructor tree =
  True

noFinalField :: ParseTree -> Bool
noFinalField tree =
  True

integerWithinRange :: ParseTree -> Bool
integerWithinRange tree =
  True

weederRules = [notAbstractFinal]

weed tree =
  any id (pam weederRules tree)

main = do
  contents <- readFile "test/joos_tree.txt"
  let tree = treeify contents
  print $ weed tree
