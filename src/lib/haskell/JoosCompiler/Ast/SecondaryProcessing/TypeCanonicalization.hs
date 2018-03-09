module JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
  ( canonicalizeProgram
  ) where

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
canonicalizeUnit program oldUnitNode@(Node (AstCompilationUnit oldUnit) _) =
  newUnitNode
  where
    newUnitNode = f oldUnitNode
    f :: AstNode -> AstNode
    f (Node (AstField Field { fieldType = Type { innerType = NamedType {unNamedType = oldTypeName}
                                               , isArray = _isArray
                                               }
                            , fieldModifiers = m
                            , fieldName = n
                            , fieldValue = v
                            }) _children) =
      Node
        (AstField
           (Field
            { fieldType = newType
            , fieldModifiers = m
            , fieldName = n
            , fieldValue = v
            })) $
      map f _children
      where
        newType = Type {innerType = (NamedType newTypeName), isArray = _isArray}
        newTypeName = canonicalize program oldUnit oldTypeName
    f (Node (AstLocalVariable Local { localType = Type { innerType = NamedType {unNamedType = oldTypeName}
                                                       , isArray = _isArray
                                                       }
                                    , localModifiers = m
                                    , localName = n
                                    , localValue = v
                                    }) _children) =
      Node
        (AstLocalVariable
           (Local
            { localType = newType
            , localModifiers = m
            , localName = n
            , localValue = v
            })) $
      map f _children
      where
        newType = Type {innerType = (NamedType newTypeName), isArray = _isArray}
        newTypeName = canonicalize program oldUnit oldTypeName
    f (Node n _children) = Node n $ map f _children
canonicalizeUnit _ _ = error "Invalid Node type in canonicalizeUnit"

-- TODO(Ahmed) This probably handles the default package wrong
--             Possibly on-demand too. No collision handling right now
canonicalize :: WholeProgram -> CompilationUnit -> Name -> Name
canonicalize program unit name
  -- If type is already canonical
  | (resolveTypeFromProgram program name /= Nothing) = name
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
      map
        (fromMaybe (error "Imported on-demand package not found") . resolvePackageFromProgram program . importPackageName)
        onDemandImports
      -- HACK: This can fail, but if it fails then the program is wrong
    singleTypePackages =
      map
        (fromMaybe (error "Imported single-type package not found") . resolvePackageFromProgram program . importPackageName)
        singleTypeImports
    singleTypePackageContainingType = find (typeIsInPackage name) singleTypePackages
    onDemandPackageContainingType = find (typeIsInPackage name) onDemandPackages

typeIsInPackage :: Name -> Package -> Bool
typeIsInPackage [n] Package {packageCompilationUnits = units}
  | match == Nothing = False
  | otherwise = typeDecl (fromJust match) /= Nothing
  where
    match = lookup n units
typeIsInPackage n Package {subPackages = subs}
  | maybePackage /= Nothing && match /= Nothing =
    typeDecl (fromJust match) /= Nothing
  | otherwise = False
  where
    pName = init n
    uName = last n
    maybePackage = lookupPackageFromSubPackageMap pName subs
    package = fromJust maybePackage
    units = packageCompilationUnits package
    match = lookup uName units

makeOnDemandImportDeclaration :: Name -> ImportDeclaration
makeOnDemandImportDeclaration name =
  ImportDeclaration
  { importPackageName = name
  , importTypeName = Nothing
  , onDemand = True
  }
