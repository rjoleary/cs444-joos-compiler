module JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
  ( canonicalizeProgram
  ) where

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

    f (Node n _children) = Node n $ map f _children
canonicalizeUnit _ _ = error "Invalid Node type in canonicalizeUnit"

-- TODO(Ahmed) This probably handles the default package wrong
--             Possibly on-demand too. No collision handling right now
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
           (resolvePackageFromProgram program) .>
           (fromMaybe (error "Imported on-demand package not found")))

    -- HACK: This can fail, but if it fails then the program is wrong
    singleTypePackages =
      singleTypeImports |>
      map (importPackageName .>
           (resolvePackageFromProgram program) .>
           (fromMaybe (error "Imported single-type package not found")))

    singleTypePackageContainingType = find (typeIsInPackage name) singleTypePackages

    onDemandPackageContainingType = find (typeIsInPackage name) onDemandPackages

typeIsInPackage :: Name -> Package -> Bool
typeIsInPackage [n] Package {packageCompilationUnits = units}
  | length matches > 1 = error "Duplicate definition for type in package"
  | otherwise = length matches == 1
  where
    matches = filter (== n) units
typeIsInPackage n Package {subPackages = subs}
  | (maybePackage /= Nothing) = if length matches > 1
                                    then error "Duplicate definition for type in package"
                                    else (length matches == 1)
  | otherwise = error $ "typeIsInPackage could not resolve package: " ++ showName pName ++ show subs
  where
    pName = init n
    uName = last n
    maybePackage = lookupPackageFromSubPackageMap pName subs
    package = fromJust maybePackage
    units = packageCompilationUnits package
    matches = filter (== uName) units

makeOnDemandImportDeclaration :: Name -> ImportDeclaration
makeOnDemandImportDeclaration name =
  ImportDeclaration
  { importPackageName = name
  , importTypeName = Nothing
  , onDemand = True
  }

canonicalizeVar :: WholeProgram -> CompilationUnit -> Variable -> Variable
canonicalizeVar
  program
  unit
  Variable { variableType = Type { innerType = NamedType {unNamedType = oldTypeName}
                                 , isArray = _isArray
                                 }
           , variableModifiers = m
           , variableName = n
           , variableValue = v
           } =
  Variable { variableType = newType
           , variableModifiers = m
           , variableName = n
           , variableValue = v
           }
  where
    newType = Type {innerType = (NamedType newTypeName), isArray = _isArray}
    newTypeName = canonicalize program unit oldTypeName
-- Not NamedType
canonicalizeVar _ _ var = var

canonicalizeTypeDecl :: WholeProgram -> CompilationUnit -> TypeDeclaration -> TypeDeclaration
canonicalizeTypeDecl
  program
  unit
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
                     , methods        = _methods
                     , constructors   = _constructors
                     }
  where
    newSuper = canonicalize program unit oldSuper
    newInterfaces = map (canonicalize program unit) oldInterfaces
    newFields = map (canonicalizeVar program unit) fields
