module JoosCompiler.Ast.Transformers.CompilationUnitTransformers
  ( compilationUnitTransformer
  ) where

import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

compilationUnitTransformer :: Transformer
compilationUnitTransformer transformedChildren t =
  AstCompilationUnit $
  CompilationUnit
  { cuPackage = getPackage transformedChildren
  , imports = getImports transformedChildren
  , classDecl = _classDecl
  , cuClassName = _className
  }
  where
    _classDecl = getClassDecl transformedChildren
    _className = className $ fromJust _classDecl

getPackage :: [AstNode] -> Maybe Name
getPackage ts
  | (length packageDeclarationNodes) > 0 = Just packageName
  | otherwise = Nothing
  where
    packageDeclarationNodes = findChildren1 isPackageDeclaration ts
    _astPackageDeclaration = rootLabel $ head packageDeclarationNodes
    packageDeclaration = astPackageDeclaration $ _astPackageDeclaration
    packageName = packageDeclarationName $ packageDeclaration

getImports :: [AstNode] -> [ImportDeclaration]
getImports ts = imports
  where
    importNodes = findChildren1 isImport ts
    imports = map (astImport . rootLabel) importNodes

getClassDecl :: [AstNode] -> Maybe ClassDeclaration
getClassDecl ts
  | length classDeclNodes > 0 = Just classDecl
  | otherwise = Nothing
  where
    classDeclNodes = findChildren1 isClassDeclaration ts
    classDecl = astClass $ rootLabel $ head $ classDeclNodes
