{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}
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
  | otherwise = newUnit
  where
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
  | isJust dynamicFieldMaybe = DynamicFieldAccess This $ variableCanonicalName dynamicField
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
    wrapClassAccess program resolvedClass restOfName
  | otherwise                       = error $ "Could not disambiguate expression: " ++ showName (n:ns)
  where
    localMaybe = Map.lookup n vars
    (resolvedClassMaybe, restOfName) = resolveAsClass program unit name
    resolvedClass =
      resolvedClassMaybe |>
      fromMaybe (error $ "resolvedClass was nothing: " ++ showName name)

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
  | otherwise = (resolvedClass, restOfName)
  where
    resolvedClasses = map (resolveTypeInProgram program) $ inits name
    resolvedClassMaybe = find isJust resolvedClasses
    resolvedClass =
      resolvedClassMaybe |>
      fromMaybe (error "resolvedClass was nothing")
    resolvedClassNameLength =
      findIndex isJust resolvedClasses |>
      fromMaybe (error "Used resolvedClassNameLength when class is Nothing")
    restOfName = drop resolvedClassNameLength name

-- Can't access a class, so turn it into a StaticFieldAccess
wrapClassAccess :: WholeProgram -> TypeDeclaration -> Name -> Expression
wrapClassAccess program typeDecl name@(n:ns)
  | isJust maybeField = wrapAccess program eType e ns
  | otherwise = error $ "could not find field: " ++ n
  where
    maybeField = findDynamicFieldInType typeDecl n
    field =
      maybeField |>
      fromMaybe (error $ "Could not access field in class" ++ showName name)
    eType = variableType field
    e = StaticFieldAccess $ variableCanonicalName field
wrapClassAccess program typeDecl [] =
  error $ "Cannot access class (No fields left): " ++ (error $ show typeDecl)

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
  | n == "length" = wrapAccess program (Type Int False) newExpression ns
  | otherwise = error $ intercalate " " ["Tried to access field", n, "in array"]
  where
    newExpression = ArrayLengthAccess oldExpression

-- Object
wrapAccess
  program
  oldExprType@(Type (NamedType t) False)
  oldExpression
  name@(n:ns) =
  error "TODO"

-- Wrong
wrapAccess program oldExprType@(Type t False) oldExpression name@(n:ns) = error "TODO"
wrapAccess _ _ _ (n:ns) = error "Called on Null or Void"
