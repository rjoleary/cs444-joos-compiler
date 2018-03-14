module JoosCompiler.Ast.Transformers.PackageTransformers
  ( packageTransformer
  ) where

import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

packageTransformer :: Transformer
packageTransformer _ t =
  AstPackageDeclaration $ PackageDeclaration $ getPackageName t

getPackageName :: TaggedParseTree -> Name
getPackageName t = extractName nameNodes
  where
    nameNodes = findChildrenByTokenName kIdentifier t
