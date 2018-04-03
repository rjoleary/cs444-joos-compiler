module JoosCompiler.Ast.Utils where

import           Control.Applicative((<|>))
import           Data.List
import           Data.Maybe
import           Data.Tree
import           Debug.DumbTrace(trace)
import qualified Debug.DumbTrace as DumbTrace
import           Flow
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Treeify

---------- Name Resolution Utils ----------

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

getTypeInProgram :: WholeProgram -> Name -> TypeDeclaration
getTypeInProgram program name =
  resolveTypeInProgram program name |>
  fromMaybe (error $ "Could not resolve type " ++ showName name)

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

resolveUnitInProgram :: WholeProgram -> Name -> Maybe CompilationUnit
resolveUnitInProgram program name =
  find ((== name) . canonicalizeUnitName) $ programCus program

resolveStaticMethodInProgram :: WholeProgram -> Name -> String -> [Type] -> [Method]
resolveStaticMethodInProgram = resolveMethodInProgram True

resolveDynamicMethodInProgram :: WholeProgram -> Name -> String -> [Type] -> [Method]
resolveDynamicMethodInProgram = resolveMethodInProgram False

resolveMethodInProgram :: Bool -> WholeProgram -> Name -> String -> [Type] -> [Method]
resolveMethodInProgram expectingStatic program typeName mName signaure =
  resolvedMethods |>
  trace ("Resolving " ++ (showName $ typeName ++ [mName]) ++ " in methods: " ++
         (program |>
          (programCus .>
           map typeDecl .>
           catMaybes .>
           map (\t -> (typeCanonicalName t, methods t)) .>
           map (\(t,ms) -> map (\m -> intercalate "." [showName t, methodName m]) ms) .>
           mconcat .>
           map ("\n  " ++) .>
           mconcat
          )))
  where
    trace = DumbTrace.trace
    typeDeclMaybe = resolveTypeInProgram program typeName
    _typeDecl = fromMaybe (error "No type declaration found for this method") typeDeclMaybe
    resolvedMethods =
      allMethods program typeName |>
      filter (\m -> (mName == methodName m && isMethodStatic m == expectingStatic))

canonicalizeUnitName :: CompilationUnit -> Name
canonicalizeUnitName unit = cuPackage unit ++ [cuTypeName unit]

-- Returns the first candidate which matches.
findOverload :: String -> [Type] -> [Method] -> Maybe Method
findOverload name args candidates =
  filterOverload name args candidates |>
  listToMaybe

filterOverload :: String -> [Type] -> [Method] -> [Method]
filterOverload name args candidates =
  filter (\x -> methodSignature x == needle) candidates
  where
    needle = methodSignature2 name args

getTypeDeclarationsFromProgram :: WholeProgram -> [TypeDeclaration]
getTypeDeclarationsFromProgram program = typeDecls
  where
    units = programCus program
    typeDecls = catMaybes $ map typeDecl units

getDynamicFieldInUnit :: WholeProgram -> CompilationUnit -> String -> Field
getDynamicFieldInUnit = getFieldInUnit False

getStaticFieldInUnit :: WholeProgram -> CompilationUnit -> String -> Field
getStaticFieldInUnit = getFieldInUnit True

getFieldInUnit :: Bool -> WholeProgram -> CompilationUnit -> String -> Field
getFieldInUnit expectingStatic program unit n =
  (findFieldInUnit expectingStatic program unit n) |>
  fromMaybe (error $ "getFieldInUnit got Nothing: " ++ (intercalate "." [cuTypeName unit, n]))

findAnyFieldInUnit :: WholeProgram -> CompilationUnit -> String -> Maybe Field
findAnyFieldInUnit program unit name = findStaticFieldInUnit  program unit name <|>
                                       findDynamicFieldInUnit program unit name

findDynamicFieldInUnit :: WholeProgram -> CompilationUnit -> String -> Maybe Field
findDynamicFieldInUnit  = findFieldInUnit False

findStaticFieldInUnit :: WholeProgram -> CompilationUnit -> String -> Maybe Field
findStaticFieldInUnit  = findFieldInUnit True

findFieldInUnit :: Bool -> WholeProgram -> CompilationUnit -> String -> Maybe Field
findFieldInUnit expectingStatic program unit n
  | isNothing maybeUnitType = Nothing
  | otherwise = findFieldInType expectingStatic program unitType n
  where
    maybeUnitType = typeDecl unit
    unitType = fromMaybe (error "unitType was nothing") maybeUnitType

findDynamicFieldInType :: WholeProgram -> TypeDeclaration -> String -> Maybe Field
findDynamicFieldInType = findFieldInType False

findStaticFieldInType :: WholeProgram -> TypeDeclaration -> String -> Maybe Field
findStaticFieldInType = findFieldInType True

findFieldInType :: Bool -> WholeProgram -> TypeDeclaration -> String -> Maybe Field
findFieldInType expectingStatic program typeDecl n
  = result |>
    trace (show allFields)
  where
    canonicalName = typeCanonicalName typeDecl
    -- the function above returns super first. We want the opposite
    allFields = directAndIndirectFields expectingStatic program canonicalName |>
                reverse
    result = find
             (\v -> (variableName v == n && expectingStatic == (isFieldStatic v)))
             allFields

getDynamicFieldInType :: WholeProgram -> TypeDeclaration -> String -> Field
getDynamicFieldInType = getFieldInType False

getStaticFieldInType :: WholeProgram -> TypeDeclaration -> String -> Field
getStaticFieldInType = getFieldInType True

getFieldInType :: Bool -> WholeProgram -> TypeDeclaration -> String -> Field
getFieldInType expectingStatic program typeDecl n =
  findFieldInType expectingStatic program typeDecl n |>
  fromMaybe (error "getFieldInType got nothing")

---------- Hierarchy Utils ----------

type TypeHierarchy = Tree TypeDeclaration

typeHierarchy :: WholeProgram -> TypeDeclaration -> TypeHierarchy
typeHierarchy wp x = typeHierarchy' x
  where
    typeHierarchy' TypeDeclaration{typeCanonicalName=["java", "lang", "Object"]} = createNode []
    typeHierarchy' TypeDeclaration{isInterface=True} = createNode $ interfaces x
    typeHierarchy' TypeDeclaration{}                 = createNode $ super x:interfaces x
    createNode xs = Node x $ map (typeHierarchy wp . getTypeInProgram wp) xs

typeHierarchyNames :: WholeProgram -> Name -> [Name]
typeHierarchyNames wp n =
  map typeCanonicalName . flatten . typeHierarchy wp . getTypeInProgram wp $ n

directAndIndirectDynamicFields :: WholeProgram -> Name -> [Variable]
directAndIndirectDynamicFields = directAndIndirectFields True

directAndIndirectFields :: Bool -> WholeProgram -> Name -> [Variable]
directAndIndirectFields expectingStatic program name = fields
  where
    resolvedType = getTypeInProgram program name
    canonicalName = typeCanonicalName resolvedType
    _super = super resolvedType
    superFields
      | canonicalName == ["java", "lang", "Object"] = []
      | otherwise = directAndIndirectFields expectingStatic program _super
    fields = superFields ++ classFields resolvedType

allMethods :: WholeProgram -> Name -> [Method]
allMethods program name = ms
  where
    ms = (directMethods program name) ++ (indirectMethods program name)

indirectMethods :: WholeProgram -> Name -> [Method]
indirectMethods program name = ms
  where
    implemented = tail $ typeHierarchyNames program name
    ms = mconcat $ map (getTypeInProgram program .> methods) implemented

directMethods :: WholeProgram -> Name -> [Method]
directMethods program name = ms
  where
    typeDecl = getTypeInProgram program name
    ms = methods typeDecl
