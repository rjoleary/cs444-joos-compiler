module JoosCompiler.Ast.Utils where

import           Data.List
import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Treeify

extractName :: [TaggedParseTree] -> Name
extractName nameNodes = map (tokenString . rootLabel) nameNodes

flattenScope :: Scope -> [Local]
flattenScope (Scope locals (Just parent)) = locals ++ flattenScope parent
flattenScope (Scope locals Nothing)       = locals

findAstChildrenByTokenName :: String -> AstNode -> [AstNode]
findAstChildrenByTokenName name t@(Node (AstTaggedToken label) children) =
  if name == tokenName label
    then t : childMatches
    else childMatches
  where
    childMatches = findAstChildrenByTokenName1 name children
findAstChildrenByTokenName name (Node _ children) = childMatches
  where
    childMatches = findAstChildrenByTokenName1 name children

findAstChildrenByTokenName1 :: String -> [AstNode] -> [AstNode]
findAstChildrenByTokenName1 name ts =
  mconcat $ map (findAstChildrenByTokenName name) ts

findAstChildByTokenName1 :: String -> [AstNode] -> AstNode
findAstChildByTokenName1 name ts
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = findAstChildrenByTokenName1 name ts

isProperPrefixOf :: Eq a => [a] -> [a] -> Bool
isProperPrefixOf l1 l2 = l1 `isPrefixOf` l2 && (length l1) < (length l2)

qualifyClassName :: CompilationUnit -> Name
qualifyClassName u@(CompilationUnit Nothing _ (Just _classDecl) _) =
  [className _classDecl]
qualifyClassName u@(CompilationUnit (Just _packageName) _ (Just _classDecl) _) =
  _packageName ++ [className _classDecl]
qualifyClassName _ = error "Can't qualify a compilation unit without a class"

resolvePackage :: Name -> AstNode -> Maybe Package
resolvePackage name (Node (AstWholeProgram (WholeProgram packages)) _) =
  case (length results) of
    0 -> Nothing
    _ -> Just $ head results
  where
    rootAList = zip (map (fromJust . packageName) packages) packages
    rootResults = lookup name rootAList
    subResults =  map ((lookup name) . subPackages) packages
    results :: [Package]
    results = catMaybes $ rootResults : subResults
resolvePackage _ _ = error "resolvePackage not run on program"

resolveClass :: Name -> AstNode -> Maybe ClassDeclaration
resolveClass [] program = Nothing
resolveClass [x] program = Nothing
resolveClass name program
  | unit == Nothing = Nothing
  | otherwise = classDecl $ fromJust unit
  where
    _className = last name
    _packageName = init name
    package = resolvePackage _packageName program
    units = packageCompilationUnits $ fromJust package
    unit = lookup _className units
