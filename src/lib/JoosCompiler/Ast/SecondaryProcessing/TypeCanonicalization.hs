{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Woverlapping-patterns #-}

module JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
  ( canonicalizeProgram
  , canonicalizeProgramMethods
  ) where

-- The most important function in this file is canonicalize

import           Flow
import           Data.List
import           Data.Maybe
import           Data.Tree
import           Debug.DumbTrace(trace)
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import qualified Data.Map.Strict as Map

javaLang :: ImportDeclaration
javaLang = makeOnDemandImportDeclaration ["java", "lang"]

-- Assumes the children are Compilation Units
canonicalizeProgram :: AstNode -> AstNode
canonicalizeProgram (Node (AstWholeProgram oldProgram@(WholeProgram packages oldUnits)) oldUnitNodes) =
  Node (AstWholeProgram newProgram) newUnitNodes
  where
    newProgram = WholeProgram packages newUnits
    newUnits = map (canonicalizeUnit oldProgram) oldUnits
    newUnitNodes =
      oldUnitNodes |>
      map (canonicalizeUnitNode oldProgram)

canonicalizeProgram _ = error "Invalid node type in canonicalizeProgram"

canonicalizeProgramMethods :: AstNode -> AstNode
canonicalizeProgramMethods (Node (AstWholeProgram oldProgram@(WholeProgram packages oldUnits)) oldUnitNodes) =
  Node (AstWholeProgram newProgram) newUnitNodes
  where
    newProgram = WholeProgram packages newUnits
    newUnits = map (canonicalizeUnitMethods oldProgram) oldUnits
    newUnitNodes =
      oldUnitNodes |>
      map (canonicalizeUnitMethodsInNode oldProgram)

canonicalizeProgramMethods _ = error "Invalid node type in canonicalizeProgram"

canonicalizeUnit :: WholeProgram -> CompilationUnit -> CompilationUnit
canonicalizeUnit program oldUnit = canonicalizedUnit
  where
    canonicalizedUnit = CompilationUnit { cuPackage = cuPackage oldUnit
                                        , imports = imports oldUnit
                                        , typeDecl = canonicalizedTypeDecl
                                        , cuTypeName = cuTypeName oldUnit
                                        }
    canonicalizedTypeDecl
      | typeDecl oldUnit == Nothing = Nothing
      | otherwise = Just (typeDecl oldUnit |>
                          fromJust |>
                          canonicalizeTypeDecl program oldUnit)

canonicalizeUnitNode :: WholeProgram -> AstNode -> AstNode
canonicalizeUnitNode program (Node (AstCompilationUnit oldUnit) _children) =
  (Node (AstCompilationUnit canonicalizedUnit) _children)
  where
    canonicalizedUnit = canonicalizeUnit program oldUnit
canonicalizeUnitNode _ _ = error "Invalid Node type in canonicalizeUnit"

canonicalizeUnitMethods :: WholeProgram -> CompilationUnit -> CompilationUnit
canonicalizeUnitMethods program oldUnit =
  oldUnit{typeDecl = newTypeDecl}
  where
    newTypeDecl
      | typeDecl oldUnit == Nothing = Nothing
      | otherwise = Just (typeDecl oldUnit |>
                          fromJust |>
                          canonicalizeTypeDeclMethods program oldUnit)

canonicalizeUnitMethodsInNode :: WholeProgram -> AstNode -> AstNode
canonicalizeUnitMethodsInNode program (Node (AstCompilationUnit oldUnit) _children) =
  (Node (AstCompilationUnit canonicalizedUnit) _children)
  where
    canonicalizedUnit = canonicalizeUnit program oldUnit
canonicalizeUnitMethodsInNode _ _ = error "Invalid Node type in canonicalizeUnit"

-- THIS IS THE MEAT
canonicalize :: WholeProgram -> CompilationUnit -> Name -> Name
canonicalize program unit name
  -- If type is already canonical
  | resolveTrace (showName name ++ " already Canonical") $ isJust resolvedAsCanonical = name

  -- SingleTypeImport. We don't need to worry about collisions because they're
  -- checked elsewhere (src/compiler/NameResolution)

  | isJust singleTypePackageContainingType =
    packageName (fromJust singleTypePackageContainingType) ++ name

  | isJust onDemandPackageContainingType =
    resolveTrace "On demand result:" $ packageName (fromMaybe (error "This should never happen. Expected package to exist")
                 onDemandPackageContainingType) ++ name

  | otherwise = error $ "Could not canonicalize: " ++ showName name
  where
    resolvedAsCanonical = resolvePrefixes (resolveTypeInProgram program) name

    thisPackage = makeOnDemandImportDeclaration $ cuPackage unit

    importsWithoutDefault = imports unit

    _imports = [thisPackage] ++ importsWithoutDefault ++ [javaLang]

    onDemandImports = filter (\i -> onDemand i) _imports

    singleTypeImports = filter (\i -> not $ onDemand i) _imports

      -- HACK: This can fail, but if it fails then the program is wrong
    onDemandPackages =
      onDemandImports |>
      map (importPackageName .>
           (resolvePackageInProgram program) .>
           (fromMaybe (error $ "Imported on-demand package not found: " ++ showName name))) |>
      resolveTrace "On-demand packages: "

    -- HACK: This can fail, but if it fails then the program is wrong
    singleTypePackages =
      singleTypeImports |>
      map (importPackageName .>
           (resolvePackageInProgram program) .>
           (fromMaybe (error $ "Imported single-type package not found: " ++ showName name)))

    singleTypePackageContainingType =
      resolvePrefixes (findPackageContainingType singleTypePackages) name |>
      resolveTrace (showName name ++ " single")

    onDemandPackageContainingType =
      resolvePrefixes (findPackageContainingType onDemandPackages) name |>
      resolveTrace (showName name ++ " on-demand")

resolvePrefixes :: (Name -> Maybe a) -> Name -> Maybe a
resolvePrefixes resolve name =
  inits name |>
  map resolve |>
  catMaybes |>
  listToMaybe

resolveTrace :: Show a => String -> a -> a
resolveTrace s x = trace (intercalate ": " [s, show x]) x

findPackageContainingType :: [Package] -> Name -> Maybe Package
findPackageContainingType packages name = find (typeIsInPackage name) packages

typeIsInPackage :: Name -> Package -> Bool
typeIsInPackage [] _ = False
typeIsInPackage [n] Package {packageCompilationUnits = units}
  | length matches > 1 = error "Duplicate definition for type in package"
  | otherwise = length matches == 1
  where
    matches = filter (== n) units
-- TODO(Ahmed) harden. This was written with a different design in mind
typeIsInPackage n (Package _ units)
  | length matches > 1 = error "Duplicate definition for type in package"
  | (length matches == 1) = True
  | otherwise = False
  where
    uName = last n
    matches = filter (== uName) units

makeOnDemandImportDeclaration :: Name -> ImportDeclaration
makeOnDemandImportDeclaration name =
  ImportDeclaration
  { importPackageName = name
  , importTypeName = Nothing
  , onDemand = True
  }

canonicalizeStatement :: WholeProgram -> CompilationUnit -> VariableMap -> Statement -> Statement
canonicalizeStatement program unit formalParameters statement =
  canonicalizedReal
  where
    g :: (VariableMap -> Expression -> Expression) -> VariableMap -> Statement -> Statement
    g fn vars old@LocalStatement{localVariable = v} = new {localVariable = newVar}
      where
      new = mapStatementVarsExpression fn vars old
      newVar = canonicalizeVar program unit vars v
    g fn vars old = mapStatementVarsExpression fn vars old
    canonicalizedReal =
      statement |>
      mapStatementVars (g $ canonicalizeExpression program unit) formalParameters

canonicalizeExpression :: WholeProgram -> CompilationUnit -> VariableMap -> Expression -> Expression
canonicalizeExpression program unit vars (CastExpression oldType e) =
  CastExpression newType e
  where
    newType = canonicalizeType program unit oldType

canonicalizeExpression program unit vars (NewArrayExpression oldType e) =
  NewArrayExpression newType e
  where
    newType = canonicalizeType program unit oldType

canonicalizeExpression program unit vars (InstanceOfExpression e oldType) =
  InstanceOfExpression e newType
  where
    newType = canonicalizeType program unit oldType

canonicalizeExpression program unit vars (ExpressionName name) =
  ExpressionName $ canonicalizeNameInExpression program unit vars name

canonicalizeExpression program unit vars (NewExpression name e) =
  (NewExpression (canonicalizeNameInExpression program unit vars name) $ e) |>
  trace ("Canonicalizing new: " ++ showName name)

-- Do not need canonicalization
-- Explicitly list expression types to expose bugs
canonicalizeExpression program unit _ old@BinaryOperation{} = old
canonicalizeExpression program unit _ old@UnaryOperation{} = old
canonicalizeExpression program unit _ old@LiteralExpression{} = old
canonicalizeExpression program unit _ old@This{} = old
canonicalizeExpression program unit _ old@ArrayExpression{} = old
canonicalizeExpression program unit _ old@AmbiguousFieldAccess{} = old
-- Those are only available after disambiguation
canonicalizeExpression program unit _ old@DynamicMethodInvocation{} = old
canonicalizeExpression program unit _ old@DynamicFieldAccess{} = old
canonicalizeExpression program unit _ old@StaticMethodInvocation{} = old
canonicalizeExpression program unit _ old@StaticFieldAccess{} = old
canonicalizeExpression program unit _ old@LocalAccess{} = old
canonicalizeExpression program unit _ old@ClassAccess{} = old

canonicalizeNameInExpression :: WholeProgram -> CompilationUnit -> VariableMap -> Name -> Name
canonicalizeNameInExpression program unit vars (n:ns)
  | isJust $ Map.lookup n $ trace ("Canonicalizing " ++ n ++ ": " ++ show vars) vars = n:ns
  | isJust $ findAnyFieldInUnit program unit n = n:ns
  | otherwise = canonicalize program unit (n:ns)
  where
    fieldNames = map variableName $ classFields unitType
    unitType = fromMaybe (error "Expected unitType to exist") $ typeDecl unit
canonicalizeNameInExpression program unit vars [] = error "Invalid Name in Expression"

canonicalizeMethod :: WholeProgram -> CompilationUnit -> Method -> Method
canonicalizeMethod program unit old@Method{ methodName = n
                                          , methodParameters = oldParameters
                                          , methodReturn        = oldRet
                                          } =
  old { methodCanonicalName = canonicalizeMethodName unit n
      , methodStatement     = newStatement
      , methodParameters    = newParameters
      , methodReturn        = newReturn
      }
  where
    statement = methodStatement old
    formalParameters = Map.fromList $ map (\v -> (variableName v, v)) newParameters
    newParameters = map (canonicalizeVar program unit Map.empty) oldParameters
    newStatement = canonicalizeStatement program unit formalParameters statement
    newReturn = canonicalizeType program unit oldRet

canonicalizeVar :: WholeProgram -> CompilationUnit -> VariableMap -> Variable -> Variable
canonicalizeVar
  program
  unit
  vars
  old@Variable{ variableType = oldType
              , variableName = n
              , variableValue = oldValue
           } =
  old { variableType = newType
      , variableCanonicalName = canonicalizeVariableName unit n
      , variableValue = canonicalizedExpression
      }
  where
    newType = canonicalizeType program unit oldType
    canonicalizedExpression = mapExpression (canonicalizeExpression program unit vars) oldValue

canonicalizeType :: WholeProgram -> CompilationUnit -> Type -> Type
canonicalizeType program unit (Type NamedType{ unNamedType = oldTypeName } isArr) =
  Type (NamedType newTypeName) isArr
  where
    newTypeName = canonicalize program unit oldTypeName
canonicalizeType program unit t = t

canonicalizeTypeDecl :: WholeProgram -> CompilationUnit -> TypeDeclaration -> TypeDeclaration
canonicalizeTypeDecl
  program
  unit@(CompilationUnit pName _ _ _)
  TypeDeclaration { typeName = name
                  , classModifiers = modifiers
                  , isInterface    = _isInterface
                  , super          = oldSuper
                  , interfaces     = oldInterfaces
                  , classFields    = fields
                  , methods        = _methods
                  , constructors   = _constructors
                  } =
  TypeDeclaration { typeName = name
                  , classModifiers = modifiers
                  , isInterface    = _isInterface
                  , super          = newSuper
                  , interfaces     = newInterfaces
                  , classFields    = fields
                  , methods        = _methods
                  , constructors   = _constructors
                  , typeCanonicalName = _typeCanonicalName
                  }
  where
    newSuper = canonicalize program unit oldSuper
    newInterfaces = map (canonicalize program unit) oldInterfaces
    _typeCanonicalName = pName ++ [name]
canonicalizeTypeDecl _ EmptyFile{} _ = error "Tried to canonicalize type in empty file"

canonicalizeTypeDeclMethods :: WholeProgram -> CompilationUnit -> TypeDeclaration -> TypeDeclaration
canonicalizeTypeDeclMethods
  program
  unit@(CompilationUnit pName _ _ _)
  old@TypeDeclaration { methods = oldMethods
                      , constructors = oldConstructors
                      , classFields = fields
                      } =
  old { methods = newMethods
      , constructors = newConstructors
      , classFields = newFields
      }
  where
    newMethods = map (canonicalizeMethod program unit) oldMethods
    newConstructors = map (canonicalizeMethod program unit) oldConstructors
    newFields = map (canonicalizeVar program unit Map.empty) fields
canonicalizeTypeDeclMethods _ EmptyFile{} _ = error "Tried to canonicalize methods in empty file"

canonicalizeMethodName :: CompilationUnit -> String -> Name
canonicalizeMethodName = canonicalizeVariableName

canonicalizeVariableName :: CompilationUnit -> String -> Name
canonicalizeVariableName unit n = canonicalPrefix ++ [n]
  where
    unitType = fromMaybe (error "unitType was Nothing") $ typeDecl unit
    canonicalPrefix = cuPackage unit ++ [typeName unitType]
