module JoosCompiler.Ast.Transformers.CompilationUnitTransformers
  ( compilationUnitTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

compilationUnitTransformer :: Transformer
compilationUnitTransformer transformedChildren t =
  AstCompilationUnit $
  CompilationUnit
  { package = getPackage transformedChildren
  , imports = getImports transformedChildren
  , classDecl = getClassDecl transformedChildren
  }

getPackage :: [AstNode] -> Maybe Name
getPackage ts
  | (length packageNodes) > 0 = Just packageName
  | otherwise = Nothing
  where
    packageNodes = findChildren1 isPackage ts
    package = head packageNodes
    packageName = [] -- packageName $ astPackage $ rootLabel package

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
