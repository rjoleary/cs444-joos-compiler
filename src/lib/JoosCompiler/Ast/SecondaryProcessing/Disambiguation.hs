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
disambiguate (Node p@(AstWholeProgram program) units) =
  Node p $ map (disambiguateUnit program) units
disambiguate _ = error "Invalid node type in disambiguate"

disambiguateUnit :: WholeProgram -> AstNode -> AstNode
disambiguateUnit program t@(Node (AstCompilationUnit oldUnit) children)
  | maybeOldTypeDecl == Nothing = t
  | otherwise = Node (AstCompilationUnit newUnit) newChildren
  where
    maybeOldTypeDecl = typeDecl oldUnit
    oldTypeDecl = fromMaybe (error "oldTypeDecl was Nothing") maybeOldTypeDecl
    newTypeDecl = oldTypeDecl {methods = newMethods}
    newUnit = oldUnit {typeDecl = Just newTypeDecl}
    newMethods = map (disambiguateMethod program oldUnit) $ methods oldTypeDecl
    newChildren = map (disambiguateTree program oldUnit) children
disambiguateUnit _ _ = error "Wrong node type in disambiguateUnit"

disambiguateMethod :: WholeProgram -> CompilationUnit -> Method -> Method
disambiguateMethod program unit method =
  method { methodStatement = newStatement }
  where
    oldStatement = methodStatement method
    newStatement = disambiguateStatement program unit oldStatement

disambiguateStatement :: WholeProgram -> CompilationUnit -> Statement -> Statement
disambiguateStatement program unit statement = newStatement
  where
    newStatement = mapStatementVarsExpression f Map.empty statement
    f :: VariableMap -> Expression -> Expression
    f vars old = disambiguateExpression program unit vars old

disambiguateExpression :: WholeProgram -> CompilationUnit -> VariableMap -> Expression -> Expression
disambiguateExpression program unit vars e@(ExpressionName [n])
  | isJust localMaybe = LocalAccess local
  | isJust staticFieldMaybe = StaticFieldAccess staticField
  | isJust dynamicFieldMaybe = DynamicFieldAccess This dynamicField
  | otherwise = error $ "Could not disambiguate expression: " ++  show e
  where
    localMaybe = Map.lookup n $ trace (show vars) vars
    local      = fromMaybe (error $ intercalate " " ["Local", n, "not found"]) localMaybe
    staticField = fromMaybe (error $ intercalate " " ["Field", n, "Not found"]) staticFieldMaybe
    staticFieldMaybe
      | maybeUnitType == Nothing = Nothing
      | otherwise =
        unitType |>
        classFields |>
        find (\v -> (variableName v == n && isFieldStatic v))
    dynamicField = fromMaybe (error $ intercalate " " ["Field", n, "Not found"]) dynamicFieldMaybe
    dynamicFieldMaybe
      | isNothing maybeUnitType = Nothing
      | otherwise =
        unitType |>
        classFields |>
        find (\v -> (variableName v == n && not (isFieldStatic v)))
    maybeUnitType = typeDecl unit
    unitType = fromMaybe (error "unitType was nothing") maybeUnitType
disambiguateExpression _ _ _ e = e

disambiguateTree :: WholeProgram -> CompilationUnit -> AstNode -> AstNode
disambiguateTree _ _ = id
