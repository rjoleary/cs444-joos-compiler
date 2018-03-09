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

resolvePackageFromProgram :: WholeProgram -> Name -> Maybe Package
resolvePackageFromProgram (WholeProgram subPackage _) _name =
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

resolveTypeFromProgram :: WholeProgram -> Name -> Maybe TypeDeclaration
resolveTypeFromProgram _ [] = Nothing
resolveTypeFromProgram _ [_] = Nothing
resolveTypeFromProgram program@(WholeProgram _ cus) name
  | package == Nothing = Nothing
  | length matchingUnits > 1 = error "resolveTypeFromProgram found two matching compilation units"
  | length matchingUnits == 0 = Nothing
  | otherwise = result
  where
    _typeName = last name
    _packageName = init name
    package = resolvePackageFromProgram program _packageName
    units = packageCompilationUnits $ fromJust package
    matchingUnits = filter (== _typeName) units
    unit = fromMaybe (error "Could not find unit in resolveTypeFromProgram") $ find (\unit -> cuTypeName unit == _typeName) cus
    result = typeDecl unit

resolveInScope :: WholeProgram -> Scope -> String -> Either Field Local
resolveInScope program scope name
  | localMatch /= Nothing = Right $ fromJust localMatch
  | resolvedField /= Nothing = Left $ fromJust resolvedField
  | otherwise = error "Cannot resolve in scope"
  where
    localMatch = find (\l -> name == localName l) locals
    locals = flattenScope scope
    -- unitTypeName is the name of the class that this scope belongs to
    unitTypeName = scopeCuName scope
    maybeUnit = find ((== unitTypeName) . canonicalizeUnitName) $ programCus program
    unit = fromJust maybeUnit
    resolvedField = resolveFieldInProgramUnit program unit name

resolveFieldInProgramUnit :: WholeProgram -> CompilationUnit -> String -> Maybe Field
resolveFieldInProgramUnit program unit name
  | fieldMaybe /= Nothing = Just field
  | superMaybe /= Nothing = resolveFieldInProgramUnit program superUnit name
  | otherwise = error "Cannot resolveFieldInProgramUnit"
  where
    units = programCus program
    thisTypeMaybe = typeDecl unit
    thisType = fromJust thisTypeMaybe
    fieldMaybe = find ((== name) . fieldName) $ classFields thisType
    field = fromJust fieldMaybe
    superMaybe = find ((== super thisType) . canonicalizeUnitName) $ programCus program
    superUnit = fromJust superMaybe

-- TODO(Ahmed)
resolveMethod :: Name -> Method
resolveMethod _ = Method _type [] "method" [] []
  where _type = Type Int False

canonicalizeUnitName :: CompilationUnit -> Name
canonicalizeUnitName unit = cuPackage unit ++ [cuTypeName unit]
