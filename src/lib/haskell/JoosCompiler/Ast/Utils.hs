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

qualifyTypeName :: CompilationUnit -> Name
qualifyTypeName u@(CompilationUnit Nothing _ (Just _typeDecl) _) =
  [typeName _typeDecl]
qualifyTypeName u@(CompilationUnit (Just _packageName) _ (Just _typeDecl) _) =
  _packageName ++ [typeName _typeDecl]
qualifyTypeName _ = error "Can't qualify a compilation unit without a class"

resolvePackageFromProgram :: WholeProgram -> Name -> Maybe Package
resolvePackageFromProgram (WholeProgram subPackage) _name =
  lookupPackage _name subPackage

lookupPackageFromSubPackageMap :: Name -> SubPackageMap -> Maybe Package
lookupPackageFromSubPackageMap name m
  | result == Nothing = Nothing
  | otherwise = lookupPackage tailOfName $ fromJust result
  where
    headOfname = head name
    tailOfName = tail name
    result = lookup headOfname m

lookupPackage :: Name -> SubPackage -> Maybe Package
lookupPackage [] (SubPackage p _) = p
lookupPackage name (SubPackage _ m) = lookupPackageFromSubPackageMap name m

resolveTypeFromProgram :: Name -> WholeProgram -> Maybe TypeDeclaration
resolveTypeFromProgram [] program = Nothing
resolveTypeFromProgram [x] program = Nothing
resolveTypeFromProgram name program
  | package == Nothing = Nothing
  | unit == Nothing = Nothing
  | otherwise = typeDecl $ fromJust unit
  where
    _typeName = last name
    _packageName = init name
    package = resolvePackageFromProgram program _packageName
    units = packageCompilationUnits $ fromJust package
    unit = lookup _typeName units

-- TODO(Ahmed)
resolveMethod :: Name -> Method
resolveMethod _ = Method _type [] "method" [] []
  where _type = Type Int False
