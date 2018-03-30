module JoosCompiler.Ast.SecondaryProcessing.Disambiguation
  (disambiguate) where

import           Data.List
import           Data.Maybe
import           Data.Tree
import           Debug.Trace(trace)
import           Flow
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import qualified Data.Map.Strict as Map

{- TODO
    - traverse tree until we see expression name
    - if expression name, try to resolve first part as local
    - otherwise,          try to resolve as field (double check the order)
    - otherwise,          try to resolve as an import
    - otherwise,          try to resolve as a package
      in this case, we keep resolving as a package until we hit a type

   Once it's resolved, we recursively resolve the rest of the name. Needs to be
   a type name in order for the resolution to be valid. We then generate a tree
   of "accesses" (staticFieldAccess, fieldAccess, localAccess)
-}

disambiguate :: AstNode -> AstNode
disambiguate (Node p@(AstWholeProgram oldProgram) unitNodes) =
  Node (AstWholeProgram newProgram) $ map (disambiguateUnitNode oldProgram) unitNodes
  where
    newProgram = oldProgram { programCus = newUnits }
    newUnits = map (disambiguateUnit oldProgram) oldUnits
    oldUnits = programCus oldProgram
disambiguate _ = error "Invalid node type in disambiguate"

disambiguateUnitNode :: WholeProgram -> AstNode -> AstNode
disambiguateUnitNode program t@(Node (AstCompilationUnit oldUnit) children) =
  Node (AstCompilationUnit newUnit) children
  where
    newUnit = disambiguateUnit program oldUnit
disambiguateUnitNode _ _ = error "Wrong node type in disambiguateUnit"

disambiguateUnit :: WholeProgram -> CompilationUnit -> CompilationUnit
disambiguateUnit program oldUnit
  | maybeOldTypeDecl == Nothing = oldUnit
  | otherwise = newUnit
  where
    maybeOldTypeDecl = typeDecl oldUnit
    oldTypeDecl = fromMaybe (error "oldTypeDecl was Nothing") maybeOldTypeDecl
    newTypeDecl = oldTypeDecl {methods = newMethods}
    newUnit = oldUnit {typeDecl = Just newTypeDecl}
    newMethods = map (disambiguateMethod program oldUnit) $ methods oldTypeDecl

disambiguateMethod :: WholeProgram -> CompilationUnit -> Method -> Method
disambiguateMethod program unit method =
  method { methodStatement = newStatement }
  where
    oldStatement = methodStatement method
    newStatement = disambiguateStatement program unit method oldStatement

disambiguateStatement :: WholeProgram -> CompilationUnit -> Method -> Statement -> Statement
disambiguateStatement program unit method statement = newStatement
  where
    formalParameters = methodParameters method
    formalParametersMap = Map.fromList $ map (\v -> (variableName v, v)) formalParameters
    newStatement = mapStatementVarsExpression f formalParametersMap statement
    f :: VariableMap -> Expression -> Expression
    f vars old = disambiguateExpression program unit vars old

disambiguateExpression :: WholeProgram -> CompilationUnit -> VariableMap -> Expression -> Expression
disambiguateExpression program unit vars e@(ExpressionName [n])
  | isJust localMaybe = LocalAccess $ variableName local
  | isJust staticFieldMaybe = StaticFieldAccess $ variableCanonicalName staticField
  | isJust dynamicFieldMaybe = DynamicFieldAccess This $ variableCanonicalName dynamicField
  | otherwise = error $ "Could not disambiguate expression: " ++  show e
  where
    localMaybe = Map.lookup n vars
    local      = fromMaybe (error $ intercalate " " ["Local", n, "not found"]) localMaybe

    staticField = getStaticFieldInUnit unit n
    staticFieldMaybe = findStaticFieldInUnit unit n

    dynamicField = getDynamicFieldInUnit unit n
    dynamicFieldMaybe = findDynamicFieldInUnit unit n

disambiguateExpression program unit vars e@(ExpressionName name@(n:ns))
  | isJust localMaybe               = e
  | staticFieldExistsInUnit unit n  = e
  | dynamicFieldExistsInUnit unit n = e
  | isJust resolvedClass            = e
  | otherwise                       = error $ "Could not disambiguate expression: " ++ showName (n:ns)
  where
    localMaybe = Map.lookup n vars
    (resolvedClass, restOfName) = resolveAsClass program unit name

disambiguateExpression _ _ _ e = e

staticFieldExistsInUnit :: CompilationUnit -> String -> Bool
staticFieldExistsInUnit = fieldExistsInUnit True

dynamicFieldExistsInUnit :: CompilationUnit -> String -> Bool
dynamicFieldExistsInUnit = fieldExistsInUnit False

fieldExistsInUnit :: Bool -> CompilationUnit -> String -> Bool
fieldExistsInUnit expectingStatic unit name =
  isJust $ findFieldInUnit expectingStatic unit name

getDynamicFieldInUnit :: CompilationUnit -> String -> Field
getDynamicFieldInUnit = getFieldInUnit False

getStaticFieldInUnit :: CompilationUnit -> String -> Field
getStaticFieldInUnit = getFieldInUnit True

getFieldInUnit :: Bool -> CompilationUnit -> String -> Field
getFieldInUnit expectingStatic unit n =
  fromMaybe (error "getFieldInUnit got Nothing") (findFieldInUnit expectingStatic unit n)

findDynamicFieldInUnit :: CompilationUnit -> String -> Maybe Field
findDynamicFieldInUnit  = findFieldInUnit False

findStaticFieldInUnit :: CompilationUnit -> String -> Maybe Field
findStaticFieldInUnit  = findFieldInUnit True

findFieldInUnit :: Bool -> CompilationUnit -> String -> Maybe Field
findFieldInUnit expectingStatic unit n
  | isNothing maybeUnitType = Nothing
  | otherwise =
    unitType |>
    classFields |>
    find (\v -> (variableName v == n && expectingStatic == (isFieldStatic v)))
  where
    maybeUnitType = typeDecl unit
    unitType = fromMaybe (error "unitType was nothing") maybeUnitType

-- newName is the part of the name left after resolving as class
resolveAsClass :: WholeProgram -> CompilationUnit -> Name -> (Maybe TypeDeclaration, Name)
resolveAsClass program unit name = (resolvedClass, newName)
  where
    resolvedClass = resolveTypeInProgram program name
    newName = name
