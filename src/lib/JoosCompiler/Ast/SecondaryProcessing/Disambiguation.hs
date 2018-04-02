{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}

module JoosCompiler.Ast.SecondaryProcessing.Disambiguation
  (disambiguate) where

import           Data.List
import           Data.Maybe
import           Data.Tree
import           Debug.Trace(trace)
import qualified Debug.DumbTrace as DumbTrace
import           Flow
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import qualified Data.Map.Strict as Map

disambiguate :: AstNode -> AstNode
disambiguate (Node p@(AstWholeProgram oldProgram) unitNodes) =
  Node (AstWholeProgram newProgram) $ map (disambiguateUnitNode oldProgram) newUnitNodes
  where
    newProgram = oldProgram { programCus = newUnits }
    newUnits = map (disambiguateUnit oldProgram) oldUnits
    oldUnits = programCus oldProgram
    newUnitNodes = map (disambiguateUnitNode oldProgram) unitNodes
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
  | otherwise = newUnit |> trace ("Disambiguating unit: " ++ cuTypeName oldUnit)
  where
    trace = DumbTrace.trace
    maybeOldTypeDecl = typeDecl oldUnit
    oldTypeDecl = fromMaybe (error "oldTypeDecl was Nothing") maybeOldTypeDecl
    newUnit = oldUnit {typeDecl = Just newTypeDecl}
    newMethods = map (disambiguateMethod program oldUnit) $ methods oldTypeDecl
    newFields = map (disambiguateVariable program oldUnit) $ classFields oldTypeDecl
    newTypeDecl = oldTypeDecl { methods = newMethods
                              , classFields = newFields
                              }

disambiguateVariable program unit variable =
  variable { variableValue = newValue }
  where
    value = variableValue variable
    newValue = mapExpression (disambiguateExpression program unit Map.empty) value

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
    newStatement = mapStatementVars (mapStatementVarsExpression f) formalParametersMap statement
    f :: VariableMap -> Expression -> Expression
    f vars old = disambiguateExpression program unit vars old

disambiguateExpression :: WholeProgram -> CompilationUnit -> VariableMap -> Expression -> Expression
disambiguateExpression program unit vars e@(ExpressionName [n])
  | isJust localMaybe = LocalAccess $ variableName local
  | isJust staticFieldMaybe = StaticFieldAccess $ variableCanonicalName staticField
  | isJust dynamicFieldMaybe = DynamicFieldAccess This  n
  | otherwise = e --  error $ "Could not disambiguate expression: " ++  show e
  where
    localMaybe = Map.lookup n vars
    local      = fromMaybe (error $ intercalate " " ["Local", n, "not found"]) localMaybe

    staticField = getStaticFieldInUnit unit n
    staticFieldMaybe = findStaticFieldInUnit unit n

    dynamicField = getDynamicFieldInUnit unit n
    dynamicFieldMaybe = findDynamicFieldInUnit unit n

disambiguateExpression program unit vars e@(ExpressionName name@(n:ns))
  | isJust localMaybe               = (LocalAccess n)
  | staticFieldExistsInUnit unit n  = e
  | dynamicFieldExistsInUnit unit n = e
  | isJust resolvedClassMaybe       =
    wrapClassAccess program resolvedClass restOfName |>
    trace ("Wrapping class access: " ++ showName name)
  | otherwise                       = error $ "Could not disambiguate expression: " ++ showName (n:ns)
  where
    trace = DumbTrace.trace
    localMaybe = Map.lookup n vars
    (resolvedClassMaybe, restOfName) = resolveAsClass program unit name
    resolvedClass =
      resolvedClassMaybe |>
      fromMaybe (error $ "resolvedClass was nothing: " ++ showName name)

disambiguateExpression program unit vars old@(DynamicMethodInvocation (ClassAccess cName) mName args) =
  StaticMethodInvocation cName mName args

disambiguateExpression program unit vars old@(DynamicMethodInvocation (ExpressionName cName) mName args) =
  error "Ambiguous (ExpressionName) in method invocation"

disambiguateExpression program unit vars old@(DynamicMethodInvocation AmbiguousFieldAccess{} mName args) =
  error "AmbiguousFieldAccess in method invocation"

disambiguateExpression _ _ _ e = e

staticFieldExistsInUnit :: CompilationUnit -> String -> Bool
staticFieldExistsInUnit = fieldExistsInUnit True

dynamicFieldExistsInUnit :: CompilationUnit -> String -> Bool
dynamicFieldExistsInUnit = fieldExistsInUnit False

fieldExistsInUnit :: Bool -> CompilationUnit -> String -> Bool
fieldExistsInUnit expectingStatic unit name =
  isJust $ findFieldInUnit expectingStatic unit name

disambiguateTree :: WholeProgram -> CompilationUnit -> AstNode -> AstNode
disambiguateTree _ _ = id

-- newName is the part of the name left after resolving as class
resolveAsClass :: WholeProgram -> CompilationUnit -> Name -> (Maybe TypeDeclaration, Name)
resolveAsClass program unit name
  | isNothing resolvedClassMaybe = (Nothing, name)
  | otherwise =
    (resolvedClass, restOfName) |>
    trace ("name: " ++ showName name)
    trace ("resolvedClasses: " ++ show resolvedClasses)
    trace ("resolvedClass: " ++ show resolvedClass)
    trace ("restOfName: " ++ showName restOfName)
  where
    trace = DumbTrace.trace
    resolvedClasses = map (resolveTypeInProgram program) $ inits name
    resolvedClassMaybe = find isJust resolvedClasses
    resolvedClass =
      resolvedClassMaybe |>
      fromMaybe (error "resolvedClass was nothing")
    resolvedClassNameLength =
      findIndex isJust resolvedClasses |>
      fromMaybe (error "Used resolvedClassNameLength when class is Nothing")
    restOfName = drop resolvedClassNameLength name

wrapClassAccess :: WholeProgram -> TypeDeclaration -> Name -> Expression
wrapClassAccess program typeDecl name@(n:ns)
  | isJust maybeField =
    wrapAccess program eType e ns |>
    trace ("Class access: " ++ (showName name))
  | otherwise = error $ "could not find field: " ++ n
  where
    trace = DumbTrace.trace
    maybeField = findDynamicFieldInType typeDecl n
    field =
      maybeField |>
      fromMaybe (error $ "Could not access field in class" ++ showName name)
    eType = variableType field
    e = StaticFieldAccess $ variableCanonicalName field
wrapClassAccess program typeDecl [] =
  ClassAccess $ typeCanonicalName typeDecl

-- This is used for multi-part names
-- If we have something like a.b.c, then we use this to return:
--
-- c
--  `b
--    `a
--
-- This is done by finding a then wrapping it with b, then wrapping
-- that with c
wrapAccess :: WholeProgram -> Type -> Expression -> Name -> Expression
wrapAccess _ _ oldExpression [] = oldExpression

-- Array
wrapAccess
  program
  oldExprType@(Type t True)
  oldExpression
  name@(n:ns)
  | n == "length" =
    wrapAccess program (Type Int False) newExpression ns |>
    trace (showName name)
  | otherwise = error $ intercalate " " ["Tried to access field", n, "in array"]
  where
    trace = DumbTrace.trace
    newExpression = ArrayLengthAccess oldExpression

-- Object
wrapAccess
  program
  oldExprType@(Type (NamedType t) False)
  oldExpression
  name@(n:ns) =
    wrapAccess program fieldType newExpression ns |>
    trace (showName name)
  where
    maybeTypeDecl = resolveTypeInProgram program t
    typeDecl =
      maybeTypeDecl |>
      fromMaybe (error $ "typeDecl was Nothing: " ++ showName t)
    field = getDynamicFieldInType typeDecl n
    fieldType = variableType field
    newExpression = (DynamicFieldAccess oldExpression n)

-- Wrong
wrapAccess program oldExprType@(Type t False) oldExpression name@(n:ns) =
  error $ "Invalid type for access" ++ show t
wrapAccess _ _ _ (n:ns) = error "Called on Null or Void"
