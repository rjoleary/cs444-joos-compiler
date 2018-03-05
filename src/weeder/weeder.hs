import           Control.Monad
import           Data.List
import           Data.Tree
import           System.Environment
import           JoosCompiler.Exit
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

-- There are two types of weeder rules:
-- 1. The simpler variety depend solely on the token names. These rules use
--    UntaggedParseTree consisting of UntaggedToken.
-- 2. After these rules are verified, extra attributes (start/end index) are
--    attached to the tree and more weeder rules are checked. These rules use
--    TaggedParseTree and TaggedToken. Tagged token containing enough
--    information to rescue the original token string from the input file.
maxPositive :: Integer
maxPositive = 2147483647

maxNegative :: Integer
maxNegative = 2147483648

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

findChildren :: String -> UntaggedParseTree -> [UntaggedParseTree]
findChildren childName tree
  | (rootLabel tree) == childName = [tree]
  | otherwise = mconcat $ map (findChildren childName) $ subForest tree

findChildren1 :: String -> TaggedParseTree -> [TaggedParseTree]
findChildren1 childName tree
  | tokenName (rootLabel tree) == childName = [tree]
  | otherwise = mconcat $ map (findChildren1 childName) $ subForest tree

findDirectChildren1 :: String -> String -> TaggedParseTree -> [TaggedParseTree]
findDirectChildren1 childName indirectChildName tree
  | tokenName (rootLabel tree) == childName = [tree]
  | tokenName (rootLabel tree) == indirectChildName = []
  | otherwise =
    mconcat $
    map (findDirectChildren1 childName indirectChildName) $ subForest tree

getClassModifiersFromDeclaration :: UntaggedParseTree -> UntaggedParseTree
getClassModifiersFromDeclaration tree =
  head $ filter (\x -> (rootLabel x) == kModifiers) $ subForest tree

getMethodModifiersFromDeclaration :: UntaggedParseTree -> UntaggedParseTree
getMethodModifiersFromDeclaration tree = head $ findChildren kModifiers header
  where
    header = head $ findChildren kMethodHeader tree

-- Weeder rules
-- Return true if check fails
-- 1 A class cannot be both abstract and final
notAbstractFinal :: UntaggedParseTree -> Bool
notAbstractFinal tree =
  if (rootLabel tree) == kClassDeclaration
    then (hasModifier kAbstract classModifiers) &&
         (hasModifier kFinal classModifiers)
    else any id $ map notAbstractFinal $ subForest tree
  where
    classModifiers = getClassModifiersFromDeclaration tree

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

-- 8 A class/interface must be declared in a .java file with the same base name as the class/interface.
classnameSameAsFilename :: ClassName -> TaggedParseTree -> Bool
classnameSameAsFilename classname tree
  | (tokenName (rootLabel tree)) `elem`
      [kClassDeclaration, kInterfaceDeclaration] =
    classname /= getTypeNameFromDeclaration tree
  | otherwise = any (classnameSameAsFilename classname) $ subForest tree

-- 10 An interface method cannot be static, final, or native.
interfaceMethodNotStaticFinalOrNative :: UntaggedParseTree -> Bool
interfaceMethodNotStaticFinalOrNative tree =
  interface && (static || final || native)
  where
    static = not $ null $ findChildren "static" tree
    final = not $ null $ findChildren "final" tree
    native = not $ null $ findChildren "native" tree
    interface = not $ null $ findChildren "interface" tree

-- 12 Every class must contain at least one explicit constructor.
-- This works because Joos has at most one type per file.
classAtLeastOneConstructor :: UntaggedParseTree -> Bool
classAtLeastOneConstructor tree = null constructors && not (null classes)
  where
    constructors = findChildren "ConstructorDeclaration" tree
    classes = findChildren "ClassDeclaration" tree

-- 13 No field can be final.
noFinalField :: UntaggedParseTree -> Bool
noFinalField tree
  | (rootLabel tree) == kFieldDeclaration = hasModifier kFinal tree
  | otherwise = any noFinalField $ subForest tree

-- Reminder: true means this check failed (i.e. reject program)
intLiteralLessThanEqual :: Integer -> TaggedParseTree -> Bool
intLiteralLessThanEqual n tree
  | length children > 0 = n < (read $ tokenString $ rootLabel $ head $ children)
  | otherwise = False
  where
    children = findDirectChildren1 kIntLiteral kUnaryExpression tree

integerWithinRange :: ClassName -> TaggedParseTree -> Bool
integerWithinRange c tree
  | (tokenName $ rootLabel tree) == kUnaryExpression =
    if (kMinus == (tokenName $ rootLabel $ head $ children))
    -- We recurse in case we have something like (-(3))
      then (intLiteralLessThanEqual maxNegative tree) ||
           (any (integerWithinRange c) $ subForest $ children !! 1)
      else any (intLiteralLessThanEqual maxPositive) $ subForest tree
  | otherwise = any (integerWithinRange c) $ children
  where
    children = subForest tree

-- A cast operator with an expression on the left may only be a Name.
castExpression :: UntaggedParseTree -> Bool
castExpression tree =
  or $ map notValidExpression $ findChildren "CastExpression" tree
  where
    notValidExpression (Node _ ((Node "(" _):n@(Node "Expression" xs):_)) =
      not (isPrefixOf prefix (flatten n))
    notValidExpression _ = False
    prefix =
      [ "Expression"
      , "AssignmentExpression"
      , "ConditionalOrExpression"
      , "ConditionalAndExpression"
      , "InclusiveOrExpression"
      , "ExclusiveOrExpression"
      , "AndExpression"
      , "EqualityExpression"
      , "RelationalExpression"
      , "AdditiveExpression"
      , "MultiplicativeExpression"
      , "UnaryExpression"
      , "UnaryExpressionNotPlusMinus"
      , "Name"
      ]

packagePrivateClass :: UntaggedParseTree -> Bool
packagePrivateClass tree
  | (rootLabel tree) == kClassDeclaration =
    not $ hasModifier kPublic classModifiers
  | otherwise = any packagePrivateClass $ subForest tree
  where
    classModifiers = getClassModifiersFromDeclaration tree

packagePrivateMethod :: UntaggedParseTree -> Bool
packagePrivateMethod tree
  | (rootLabel tree) == kMethodDeclaration =
    not $ or $ map (\x -> hasModifier x methodModifiers) [kPublic, kProtected]
  | otherwise = any packagePrivateMethod $ subForest tree
  where
    methodModifiers = getMethodModifiersFromDeclaration tree

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
  , packagePrivateClass
  , packagePrivateMethod
  ]

taggedRules :: [ClassName -> TaggedParseTree -> Bool]
taggedRules = [classnameSameAsFilename, integerWithinRange]

untaggedWeed :: UntaggedParseTree -> Bool
untaggedWeed tree = or $ map (\f -> f tree) untaggedRules

taggedWeed :: TaggedParseTree -> ClassName -> Bool
taggedWeed tree classname = or $ map (\f -> f classname tree) taggedRules

main :: IO ()
main = do
  args <- getArgs
  let classname = args!!0
  source <- readFile (args!!1)
  tokens <- readFile (args!!2)
  contents <- readFile (args!!3)
  let tree = treeify contents
  let taggedTree = tagTree tree tokens source
  --putStrLn $ drawTree (fmap show taggedTree)
  when (untaggedWeed tree) $ exitError "Bad weed"
  when (taggedWeed taggedTree classname) $ exitError "Bad weed"
