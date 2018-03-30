module JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
  ( canonicalizeProgram
  ) where

-- The most important function in this file is canonicalize

import           Flow
import           Data.List
import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

javaLang :: ImportDeclaration
javaLang = makeOnDemandImportDeclaration ["java", "lang"]

-- Assumes the children are Compilation Units
canonicalizeProgram :: AstNode -> AstNode
canonicalizeProgram (Node p@(AstWholeProgram program) oldUnits) =
  Node p newUnits
  where
    newUnits = map (canonicalizeUnit program) oldUnits
canonicalizeProgram _ = error "Invalid node type in canonicalizeProgram"

canonicalizeUnit :: WholeProgram -> AstNode -> AstNode
canonicalizeUnit program (Node (AstCompilationUnit oldUnit) _children) =
  (Node (AstCompilationUnit canonicalizedUnit) $ map f _children)
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
    f :: AstNode -> AstNode
    f (Node (AstField field) _children) =
      Node (AstField newField) (map f _children)
      where
        newField = canonicalizeVar program oldUnit field

    f (Node (AstLocalVariable local) _children) =
      Node (AstLocalVariable newLocal) (map f _children)
      where
        newLocal = canonicalizeVar program oldUnit local

    f (Node (AstExpression e) _children) =
      Node (AstExpression $ mapExpression (canonicalizeExpression program oldUnit) e) $
      map f _children

    f (Node n _children) = Node n $ map f _children
canonicalizeUnit _ _ = error "Invalid Node type in canonicalizeUnit"

-- TODO(Ahmed)
-- - canonicalize local declarations
-- - figure out if casting is valid if name shadowed by local/field
canonicalizeExpression :: WholeProgram -> CompilationUnit -> Expression -> Expression
canonicalizeExpression
  program
  unit
  (CastExpression oldType@Type{ innerType = (NamedType name) } e) =
  newExpression
  where
    newExpression = CastExpression newType e
    newType = oldType { innerType = NamedType $ canonicalize program unit name }
canonicalizeExpression _ _ e = e
-- We could also canonicalize ExpressionNames here.
-- But we don’t. Because we need to make sure the name
-- isn’t also a field, so we have to do this after
-- disambigation

-- THIS IS THE MEAT
canonicalize :: WholeProgram -> CompilationUnit -> Name -> Name
canonicalize program unit name
  -- If type is already canonical
  | (resolveTypeInProgram program name /= Nothing) = name

  -- SingleTypeImport. We don't need to worry about collisions because they're
  -- checked elsewhere (src/compiler/NameResolution)

  | (singleTypePackageContainingType /= Nothing) =
    packageName (fromJust singleTypePackageContainingType) ++ name

  | (onDemandPackageContainingType /= Nothing) =
    packageName (fromMaybe (error "This should never happen. Expected package to exist")
                 onDemandPackageContainingType) ++ name

  | otherwise = error $ "Could not canonicalize: " ++ showName name
  where
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
           (fromMaybe (error $ "Imported on-demand package not found: " ++ showName name)))

    -- HACK: This can fail, but if it fails then the program is wrong
    singleTypePackages =
      singleTypeImports |>
      map (importPackageName .>
           (resolvePackageInProgram program) .>
           (fromMaybe (error $ "Imported single-type package not found: " ++ showName name)))

    singleTypePackageContainingType = find (typeIsInPackage name) singleTypePackages

    onDemandPackageContainingType = find (typeIsInPackage name) onDemandPackages

typeIsInPackage :: Name -> Package -> Bool
typeIsInPackage [n] Package {packageCompilationUnits = units}
  | length matches > 1 = error "Duplicate definition for type in package"
  | otherwise = length matches == 1
  where
    matches = filter (== n) units
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

canonicalizeMethod :: WholeProgram -> CompilationUnit -> Method -> Method
canonicalizeMethod _ unit old@Method{methodName = n} =
  old { methodCanonicalName = canonicalizeMethodName unit n }

canonicalizeVar :: WholeProgram -> CompilationUnit -> Variable -> Variable
canonicalizeVar
  program
  unit
  old@Variable{ variableType = oldType@Type{ innerType = NamedType{ unNamedType = oldTypeName }}
              , variableName = n
           } =
  old { variableType = newType
      , variableCanonicalName = canonicalizeVariableName unit n
      }
  where
    newType = oldType{ innerType = (NamedType newTypeName) }
    newTypeName = canonicalize program unit oldTypeName
-- Not NamedType
canonicalizeVar _ unit old@Variable{ variableName = name } =
  old { variableCanonicalName = canonicalizeVariableName unit name }

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
                  , classFields    = newFields
                  , methods        = newMethods
                  , constructors   = _constructors
                  , typeCanonicalName = _typeCanonicalName
                  }
  where
    newSuper = canonicalize program unit oldSuper
    newInterfaces = map (canonicalize program unit) oldInterfaces
    newFields = map (canonicalizeVar program unit) fields
    newMethods = map (canonicalizeMethod program unit) _methods
    _typeCanonicalName = pName ++ [name]
canonicalizeTypeDecl _ EmptyFile{} _ = error "Tried to canonicalize type in empty file"

canonicalizeMethodName :: CompilationUnit -> String -> Name
canonicalizeMethodName = canonicalizeVariableName

canonicalizeVariableName :: CompilationUnit -> String -> Name
canonicalizeVariableName unit n = canonicalPrefix ++ [n]
  where
    unitType = fromMaybe (error "unitType was Nothing") $ typeDecl unit
    canonicalPrefix = cuPackage unit ++ [typeName unitType]
