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
  ImportDeclaration { importPackageName = _packageName
                    , importTypeName = _typeName
                    , onDemand = _onDemand
                    }
  where
    _onDemand = isOnDemandImport t
    _packageName
      | _onDemand = getImportName t
      | otherwise = init $ getImportName t
    _typeName
      | _onDemand = Nothing
      | otherwise = Just $ last $ getImportName t

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
