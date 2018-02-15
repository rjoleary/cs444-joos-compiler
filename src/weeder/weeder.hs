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
pam lf x = map g lf
  where
    g f = f x

kClassDeclaration :: String
kClassDeclaration = "ClassDeclaration"

kInterfaceDeclaration :: String
kInterfaceDeclaration = "InterfaceDeclaration"

kAbstract :: String
kAbstract = "abstract"

kFinal :: String
kFinal = "final"

kBlock :: String
kBlock = "Block"

kStatic :: String
kStatic = "static"

kFieldDeclaration :: String
kFieldDeclaration = "FieldDeclaration"

kMethodBody :: String
kMethodBody = "MethodBody"

kMethodDeclaration :: String
kMethodDeclaration = "MethodDeclaration"

kMethodDeclarator :: String
kMethodDeclarator = "MethodDeclarator"

kMethodHeader :: String
kMethodHeader = "MethodHeader"

kNative :: String
kNative = "native"

kVoid :: String
kVoid = "void"

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

hasChild :: String -> ParseTree -> Bool
hasChild s tree =
  if (rootLabel tree) == s
    then True
    else any (hasChild s) $ subForest tree

hasMethodBody :: ParseTree -> Bool
hasMethodBody tree
  | (lhs tree == kMethodBody) = hasChild kBlock tree
  | otherwise = any hasMethodBody (subForest tree)

hasModifier :: String -> ParseTree -> Bool
hasModifier modifier tree
  | rootLabel tree == modifier = True
  | otherwise = any (hasModifier modifier) $ subForest tree

hasMethodsWithModifiers :: [String] -> ParseTree -> Bool
hasMethodsWithModifiers modifiers tree
  | (rootLabel tree) == kMethodDeclaration =
    any (\x -> hasModifier x tree) modifiers
  | otherwise = any (hasMethodsWithModifiers modifiers) $ subForest tree

findChildren :: String -> ParseTree -> [ParseTree]
findChildren childName tree
  | (rootLabel tree) == childName = [tree]
  | otherwise = mconcat $ map (findChildren childName) $ subForest tree

-- Weeder rules
-- Return true if check fails
notAbstractFinal :: ParseTree -> Bool
notAbstractFinal tree =
  if (rootLabel tree) == kClassDeclaration
    then (hasModifier kAbstract tree) && (hasModifier kFinal tree)
    else any id $ map notAbstractFinal $ subForest tree

bodyIfNotAbstractOrNative :: ParseTree -> Bool
bodyIfNotAbstractOrNative tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kAbstract tree) || (hasModifier kNative tree)
      then False
      else not . hasMethodBody $ tree
  | otherwise = any bodyIfNotAbstractOrNative $ subForest tree

abstractMethodNotStaticOrFinal :: ParseTree -> Bool
abstractMethodNotStaticOrFinal tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kAbstract tree)
      then (hasModifier kStatic tree) || (hasModifier kFinal tree)
      else False
  | otherwise = any abstractMethodNotStaticOrFinal $ subForest tree

staticMethodNotFinal :: ParseTree -> Bool
staticMethodNotFinal tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kStatic tree)
      then hasModifier kFinal tree
      else False
  | otherwise = any staticMethodNotFinal $ subForest tree

nativeMethodStatic :: ParseTree -> Bool
nativeMethodStatic tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kNative tree)
      then not $ hasModifier kStatic tree
      else False
  | otherwise = any nativeMethodStatic $ subForest tree

voidTypeOnlyUsedAsMethodReturn :: ParseTree -> Bool
voidTypeOnlyUsedAsMethodReturn tree
  | (rootLabel tree) == kVoid = True
  | (rootLabel tree) == kMethodHeader
    -- We want to skip the return type of the method -- so check the declarator
   = hasChild kVoid declarator
  | otherwise = any voidTypeOnlyUsedAsMethodReturn $ subForest tree
  where
    declarator = head $ findChildren kMethodDeclarator tree

-- TODO
classnameSameAsFilename :: ParseTree -> Bool
classnameSameAsFilename tree = True

interfaceMethodNotStaticFinalOrNative :: ParseTree -> Bool
interfaceMethodNotStaticFinalOrNative tree
  | (rootLabel tree) == kInterfaceDeclaration =
    hasMethodsWithModifiers [kStatic, kFinal, kNative] tree
  | otherwise = any interfaceMethodNotStaticFinalOrNative $ subForest tree

-- TODO
classAtLeastOneConstructor :: ParseTree -> Bool
classAtLeastOneConstructor tree = True

noFinalField :: ParseTree -> Bool
noFinalField tree
  | (rootLabel tree) == kFieldDeclaration = not $ hasModifier kFinal tree
  | otherwise = any noFinalField $ subForest tree

-- TODO
integerWithinRange :: ParseTree -> Bool
integerWithinRange tree = True

weederRules =
  [ notAbstractFinal
  , bodyIfNotAbstractOrNative
  , abstractMethodNotStaticOrFinal
  , staticMethodNotFinal
  , nativeMethodStatic
  , voidTypeOnlyUsedAsMethodReturn
  , interfaceMethodNotStaticFinalOrNative
  , noFinalField
  ]

weed tree = any id (pam weederRules tree)

main = do
  contents <- readFile "test/joos_tree.txt"
  let tree = treeify contents
  print $ weed tree
  putStrLn $ drawTree tree
