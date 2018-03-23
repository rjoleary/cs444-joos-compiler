module JoosCompiler.Ast.Utils where

import           Data.List
import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Treeify

extractName :: [TaggedParseTree] -> Name
extractName nameNodes = map (tokenString . rootLabel) nameNodes

flattenScope :: Scope -> [Local]
flattenScope (Scope locals (Just parent) _) = locals ++ flattenScope parent
flattenScope (Scope locals Nothing _)       = locals

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
qualifyTypeName u@(CompilationUnit [] _ (Just _typeDecl) _) =
  [typeName _typeDecl]
qualifyTypeName u@(CompilationUnit _packageName _ (Just _typeDecl) _) =
  _packageName ++ [typeName _typeDecl]
qualifyTypeName _ = error "Can't qualify a compilation unit without a class"

resolvePackageInProgram :: WholeProgram -> Name -> Maybe Package
resolvePackageInProgram (WholeProgram packages _) _name =
  find ((== _name) . packageName) packages

resolveTypeInProgram :: WholeProgram -> Name -> Maybe TypeDeclaration
resolveTypeInProgram _ [] = Nothing
resolveTypeInProgram program@(WholeProgram _ cus) name
  | package == Nothing = Nothing
  | length matchingUnits > 1 = error "resolveTypeInProgram found two matching compilation units"
  | length matchingUnits == 0 = Nothing
  | otherwise = result
  where
    _typeName = last name
    _packageName = init name
    package = resolvePackageInProgram program _packageName
    units = packageCompilationUnits $ fromJust package
    matchingUnits = filter (== _typeName) units
    unit = fromMaybe (error "Could not find unit in resolveTypeInProgram") $ find (\unit -> cuTypeName unit == _typeName) cus
    result = typeDecl unit

resolveFieldInProgramUnit :: WholeProgram -> CompilationUnit -> String -> Maybe Field
resolveFieldInProgramUnit program unit name
  | fieldMaybe /= Nothing = Just field
  | superMaybe /= Nothing = resolveFieldInProgramUnit program superUnit name
  | otherwise = error "Cannot resolveFieldInProgramUnit"
  where
    units = programCus program
    thisTypeMaybe = typeDecl unit
    thisType = fromJust thisTypeMaybe
    fieldMaybe = find ((== name) . variableName) $ classFields thisType
    field = fromJust fieldMaybe
    superMaybe = resolveUnitInProgram program $ super thisType
    superUnit = fromJust superMaybe

resolveFieldInType :: WholeProgram -> Type -> Name -> Maybe Field
resolveFieldInType _ _ [] = (error "Name should never be empty")
resolveFieldInType program _type [n] = resolveFieldInProgramUnit program unit n
  where
    maybeUnit = resolveUnitInProgram program $ unNamedType $ innerType _type
    unit = fromMaybe (error "maybeUnit is nothing") maybeUnit
resolveFieldInType program _type (n:ns) = resolveFieldInType program newType ns
  where
    maybeUnit = resolveUnitInProgram program $ unNamedType $ innerType _type
    maybeField = resolveFieldInProgramUnit program unit n
    field = fromMaybe (error "mayField is nothing") maybeField
    newType = variableType field
    unit = fromMaybe (error "maybeUnit is nothing") maybeUnit

resolveUnitInProgram :: WholeProgram -> Name -> Maybe CompilationUnit
resolveUnitInProgram program name =
  find ((== name) . canonicalizeUnitName) $ programCus program

resolveMethod :: Name -> Method
resolveMethod _ = Method _type [] "method" [] TerminalStatement []
  where _type = Type Int False

canonicalizeUnitName :: CompilationUnit -> Name
canonicalizeUnitName unit = cuPackage unit ++ [cuTypeName unit]
