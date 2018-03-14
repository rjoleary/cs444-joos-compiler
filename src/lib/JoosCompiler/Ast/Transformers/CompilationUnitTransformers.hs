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
  { cuPackage = fromMaybe [] $ getPackage transformedChildren
  , imports = getImports transformedChildren
  , typeDecl = _typeDecl
  , cuTypeName = _typeName
  }
  where
    _typeDecl = getTypeDecl transformedChildren
    _typeName = typeName $ fromMaybe (error "Compilation unit without class") _typeDecl

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

getTypeDecl :: [AstNode] -> Maybe TypeDeclaration
getTypeDecl ts
  | length typeDeclNodes > 0 = Just typeDecl
  | otherwise = Nothing
  where
    typeDeclNodes = findChildren1 isTypeDeclaration ts
    typeDecl = astClass $ rootLabel $ head $ typeDeclNodes
