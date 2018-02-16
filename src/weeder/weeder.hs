import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Exit

-- There are two types of weeder rules:
-- 1. The simpler variety depend solely on the token names. These rules use
--    UntaggedParseTree consisting of UntaggedToken.
-- 2. After these rules are verified, extra attributes (start/end index) are
--    attached to the tree and more weeder rules are checked. These rules use
--    TaggedParseTree and TaggedToken. Tagged token containing enough
--    information to rescue the original token string from the input file.

type UntaggedParseTree = Tree UntaggedToken
type UntaggedToken = String

type TaggedParseTree = Tree TaggedToken
data TaggedToken = TaggedToken { tokenName :: String
                               , tokenString :: String
                               , tokenStart :: Int
                               , tokenEnd :: Int
                               }
instance Show TaggedToken where
    show (TaggedToken name str start end) =
            name
            ++ (if name == str then "" else " \"" ++ str ++ "\"")
            ++ (if start == end then "" else " (" ++ show start ++ "," ++ show end ++ ")")


singleNode :: a -> Tree a
singleNode x = Node x []

lhs :: Tree t -> t
lhs (Node x _) = x

rhs :: Tree t -> [Tree t]
rhs (Node _ x) = x

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

-- Parse the tokens from joos_tokens.txt.
type Token = (String, Int, Int)
parseTokens :: String -> [Token]
parseTokens = map ((\[x, y, z] -> (x, (read y), (read z))) . words) . lines

-- Convert an untagged tree to a tagged tree with the tokens file.
tagTree :: UntaggedParseTree -> [Token] -> TaggedParseTree
tagTree utree tokens = if remainingTokens == []
                       then taggedTree
                       else error "Remaining tokens after tagging tree"
    where (taggedTree, remainingTokens) = tagTree' utree tokens

-- Recursively convert the tree and return any extra tokens.
-- TODO: Tag inner nodes with (start, end) of their children.
tagTree' :: UntaggedParseTree -> [Token] -> (TaggedParseTree, [Token])
tagTree' (Node x []) [] = ((Node (TaggedToken x x 0 0) []), [])
tagTree' (Node x []) a@((name, start, end):ts)
    | x == name = ((Node (TaggedToken x x start end) []), ts)
    | otherwise = ((Node (TaggedToken x x 0 0) []), a)
tagTree' (Node x xs) ts = ((Node (TaggedToken x x 0 0) taggedChildren), remainingTokens)
    where mapChildren :: [Tree UntaggedToken] -> [Token] -> ([Tree TaggedToken], [Token])
          mapChildren [] ts = ([], ts)
          mapChildren (n@(Node x xs):ns) ts = (taggedChild:ns', ts')
              where (taggedChild, remainingTokens) = tagTree' n ts
                    (ns', ts') = mapChildren ns remainingTokens
          (taggedChildren, remainingTokens) = mapChildren xs ts

-- Fill the tree with
insertTokenStrings :: TaggedParseTree -> String -> TaggedParseTree
insertTokenStrings tree source = fmap mapToken tree
    where mapToken t@(TaggedToken name str start end) =
            if start == end
            then t
            else (TaggedToken name (drop start . take end $ source) start end)

hasChild :: String -> UntaggedParseTree -> Bool
hasChild s tree =
  if (rootLabel tree) == s
    then True
    else any (hasChild s) $ subForest tree

hasMethodBody :: UntaggedParseTree -> Bool
hasMethodBody tree
  | (lhs tree == kMethodBody) = hasChild kBlock tree
  | otherwise = any hasMethodBody (subForest tree)

hasModifier :: String -> UntaggedParseTree -> Bool
hasModifier modifier tree
  | rootLabel tree == modifier = True
  | otherwise = any (hasModifier modifier) $ subForest tree

hasMethodsWithModifiers :: [String] -> UntaggedParseTree -> Bool
hasMethodsWithModifiers modifiers tree
  | (rootLabel tree) == kMethodDeclaration =
    any (\x -> hasModifier x tree) modifiers
  | otherwise = any (hasMethodsWithModifiers modifiers) $ subForest tree

findChildren :: String -> UntaggedParseTree -> [UntaggedParseTree]
findChildren childName tree
  | (rootLabel tree) == childName = [tree]
  | otherwise = mconcat $ map (findChildren childName) $ subForest tree

-- Weeder rules
-- Return true if check fails
-- 1 A class cannot be both abstract and final
notAbstractFinal :: UntaggedParseTree -> Bool
notAbstractFinal tree =
  if (rootLabel tree) == kClassDeclaration
    then (hasModifier kAbstract tree) && (hasModifier kFinal tree)
    else any id $ map notAbstractFinal $ subForest tree

-- 2 A method has a body if and only if it is neither abstract nor native.
bodyIffNotAbstractOrNative :: UntaggedParseTree -> Bool
bodyIffNotAbstractOrNative tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kAbstract tree) || (hasModifier kNative tree)
      then hasMethodBody tree
      else not . hasMethodBody $ tree
  | otherwise = any bodyIffNotAbstractOrNative $ subForest tree

-- 3 An abstract method cannot be static or final.
abstractMethodNotStaticOrFinal :: UntaggedParseTree -> Bool
abstractMethodNotStaticOrFinal tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kAbstract tree)
      then (hasModifier kStatic tree) || (hasModifier kFinal tree)
      else False
  | otherwise = any abstractMethodNotStaticOrFinal $ subForest tree

-- 4 A static method cannot be final.
staticMethodNotFinal :: UntaggedParseTree -> Bool
staticMethodNotFinal tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kStatic tree)
      then hasModifier kFinal tree
      else False
  | otherwise = any staticMethodNotFinal $ subForest tree

-- 5 A native method must be static.
nativeMethodStatic :: UntaggedParseTree -> Bool
nativeMethodStatic tree
  | (rootLabel tree) == kMethodDeclaration =
    if (hasModifier kNative tree)
      then not $ hasModifier kStatic tree
      else False
  | otherwise = any nativeMethodStatic $ subForest tree

-- TODO
-- 8 A class/interface must be declared in a .java file with the same base name as the class/interface.
classnameSameAsFilename :: TaggedParseTree -> ClassName -> Bool
classnameSameAsFilename tree classname = True

-- 10 An interface method cannot be static, final, or native.
interfaceMethodNotStaticFinalOrNative :: UntaggedParseTree -> Bool
interfaceMethodNotStaticFinalOrNative tree
  | (rootLabel tree) == kInterfaceDeclaration =
    hasMethodsWithModifiers [kStatic, kFinal, kNative] tree
  | otherwise = any interfaceMethodNotStaticFinalOrNative $ subForest tree

-- 12 Every class must contain at least one explicit constructor.
-- This works because Joos has at most one type per file.
classAtLeastOneConstructor :: UntaggedParseTree -> Bool
classAtLeastOneConstructor tree = null constructors && not (null classes)
    where constructors = findChildren "ConstructorDeclaration" tree
          classes = findChildren "ClassDeclaration" tree

-- 13 No field can be final.
noFinalField :: UntaggedParseTree -> Bool
noFinalField tree
  | (rootLabel tree) == kFieldDeclaration = hasModifier kFinal tree
  | otherwise = any noFinalField $ subForest tree

-- TODO
integerWithinRange :: TaggedParseTree -> ClassName -> Bool
integerWithinRange tree _ = True

-- An cast operator with an expression on the left may only be an indentifier.
castExpression :: UntaggedParseTree -> Bool
castExpression tree = False

untaggedRules :: [UntaggedParseTree -> Bool]
untaggedRules =
  [ notAbstractFinal
  , bodyIffNotAbstractOrNative
  , abstractMethodNotStaticOrFinal
  , staticMethodNotFinal
  , nativeMethodStatic
  , interfaceMethodNotStaticFinalOrNative
  , classAtLeastOneConstructor
  , noFinalField
  , castExpression
  ]

taggedRules :: [TaggedParseTree -> ClassName -> Bool]
taggedRules =
  [-- classnameSameAsFilename
 -- , integerWithinRange
  ]

untaggedWeed :: UntaggedParseTree -> Bool
untaggedWeed tree = or $ map (\f -> f tree) untaggedRules

type ClassName = String
taggedWeed :: TaggedParseTree -> ClassName -> Bool
taggedWeed tree classname = or $ map (\f -> f tree classname) taggedRules

main :: IO ()
main = do
  classname <- readFile "test/joos_classname.txt"
  source <- readFile "test/joos_input.txt"
  tokens <- readFile "test/joos_tokens.txt"
  contents <- readFile "test/joos_tree.txt"

  let tree = treeify contents
  when (untaggedWeed tree) $ exitError "Bad weed"

  let taggedTree = insertTokenStrings (tagTree tree (parseTokens tokens)) source
  when (taggedWeed taggedTree classname) $ exitError "Bad weed"
  putStrLn $ drawTree (fmap show taggedTree)
