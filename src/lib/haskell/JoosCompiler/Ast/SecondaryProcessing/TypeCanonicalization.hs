module JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
  ( canonicalizeTypes
  ) where

import           Data.List
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

javaLang :: Name
javaLang = ["java", "lang"]

canonicalizeTypes :: AstNode -> AstNode
canonicalizeTypes t@(Node program oldUnits) = Node program newUnits
  where
    newUnits = map f oldUnits
    f :: AstNode -> AstNode
    f t@(Node (AstCompilationUnit u) _) = t
    f u = u

canonicalize :: WholeProgram -> Name -> [ImportDeclaration] -> Name
canonicalize program typeName importsWithoutDefault
  -- If type is already canonical
  | (resolveTypeFromProgram typeName program /= Nothing) = typeName
  | otherwise = typeName -- TODO
    where
      imports = importsWithoutDefault -- ++ javaLang
      packages = map (resolvePackage program . importPackageName) imports
      packageContainingType = find (typeIsInPackage typeName) packages

typeIsInPackage :: a -> b -> Bool
typeIsInPackage _ _ = True
