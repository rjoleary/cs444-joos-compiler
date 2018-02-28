module JoosCompiler.Ast.Transformers.ImportTransformers
  ( importTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

importTransformer :: Transformer
importTransformer _ t =
  AstImport $
  ImportDeclaration
  {importName = getImportName t, onDemand = isOnDemandImport t}

getImportName :: TaggedParseTree -> Name
getImportName t = extractName nameNodes
  where
    nameNodes = findChildrenByTokenName kIdentifier t

isOnDemandImport :: TaggedParseTree -> Bool
isOnDemandImport t =
  if (length matches > 0)
    then True
    else False
  where
    matches = findChildrenByTokenName kTypeImportOnDemandDeclaration t
