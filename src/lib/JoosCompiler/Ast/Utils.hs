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
    package = resolvePackageFromProgram program _packageName
    units = packageCompilationUnits $ fromJust package
    matchingUnits = filter (== _typeName) units
    unit = fromMaybe (error "Could not find unit in resolveTypeInProgram") $ find (\unit -> cuTypeName unit == _typeName) cus
    result = typeDecl unit

resolveInScope :: WholeProgram -> Scope -> Name -> Either Field Local
resolveInScope program scope name
  | localMatch /= Nothing && length name == 1 =
    Right $ fromMaybe (error "LocalMatch was nothing") localMatch
  | localMatch /= Nothing =
    Left $ fromMaybe (error "Could not resolve localMatch.field")
         $ resolveFieldInType program localMatchType $ tail name
  | resolvedField /= Nothing && length name == 1 =
    Left $ fromMaybe (error "resolvedField was nothing") resolvedField
  | resolvedField /= Nothing =
    Left $ fromMaybe (error "Could not resolve field.field")
         $ resolveFieldInType program resolvedFieldType $ tail name
  | otherwise =
    Left $ fromMaybe (error "Could not resolve in package")
         $ resolveFieldInCuPackages program unit name
  where
    firstPart = head name
    locals = flattenScope scope
    localMatch = find (\l -> firstPart == variableName l) locals
    localMatchType = variableType $ fromJust localMatch
    -- unitTypeName is the name of the class that this scope belongs to
    unitTypeName = scopeCuName scope
    maybeUnit = find ((== unitTypeName) . canonicalizeUnitName) $ programCus program
    unit = fromMaybe (error $ "resolveInScope couldn't find unit: " ++ showName unitTypeName) maybeUnit
    resolvedField = resolveFieldInProgramUnit program unit firstPart
    resolvedFieldType = variableType $ fromJust resolvedField


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

resolveFieldInSubPackage :: WholeProgram -> SubPackage -> Name -> Maybe Field
resolveFieldInSubPackage _ _ [] = error "Name should never be empty"
resolveFieldInSubPackage _ _ [_] = Nothing
resolveFieldInSubPackage program (SubPackage maybePackage subPackageMap) (n:ns)
  | foundSubPackage /= Nothing =
    resolveFieldInSubPackage program (fromJust foundSubPackage) ns
  | maybeUnitName /= Nothing = resolveFieldInProgramUnit program unit n
  | otherwise = Nothing
  where
    foundSubPackage = lookup n subPackageMap
    package = fromMaybe (error "maybePackage is nothing") maybePackage
    maybeUnitName = find (== n) $ packageCompilationUnits package
    unitName = fromMaybe (error "maybeUnitName is nothing") maybeUnitName
    canonicalUnitName = (packageName package) ++ [unitName]
    maybeUnit = resolveUnitInProgram program canonicalUnitName
    unit = fromMaybe (error "maybeUnit is nothing") maybeUnit

resolveFieldInCuPackages :: WholeProgram -> CompilationUnit -> Name -> Maybe Field
resolveFieldInCuPackages _ _ [] = error "Name should never be empty"
resolveFieldInCuPackages _ _ [_] = Nothing
resolveFieldInCuPackages program unit (n:ns)
  | length matchingSubPackageMaps == 0 = Nothing
  | length matchingSubPackageMaps == 1 =
    resolveFieldInSubPackage program subPackage ns
  | otherwise = error "Found more than one matching subpackage"
  where
    onDemandImports = filter (\i -> onDemand i) $ imports unit
    packages = map (resolvePackageFromProgram program . importPackageName) onDemandImports
    subPackageMaps = map subPackages $ catMaybes packages
    matchingSubPackageMaps = filter ((/= Nothing) . lookup n) subPackageMaps
    subPackage = fromJust $ lookup n $ head matchingSubPackageMaps

resolveUnitInProgram :: WholeProgram -> Name -> Maybe CompilationUnit
resolveUnitInProgram program name =
  find ((== name) . canonicalizeUnitName) $ programCus program

resolveMethod :: Name -> Method
resolveMethod _ = Method _type [] "method" [] TerminalStatement []
  where _type = Type Int False

canonicalizeUnitName :: CompilationUnit -> Name
canonicalizeUnitName unit = cuPackage unit ++ [cuTypeName unit]
