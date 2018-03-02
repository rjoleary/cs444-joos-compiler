module JoosCompiler.Ast.Utils where

import           Data.List
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
qualifyClassName u@(CompilationUnit Nothing _ (Just _classDecl)) =
  [className _classDecl]
qualifyClassName u@(CompilationUnit (Just _packageName) _ (Just _classDecl)) =
  _packageName ++ [className _classDecl]
qualifyClassName _ = error "Can't qualify a compilation unit without a class"
