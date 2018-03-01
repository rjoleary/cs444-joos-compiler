module JoosCompiler.Ast.Transformers.BlockTransformers
  ( blockTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

blockTransformer :: Transformer
blockTransformer transformedChildren t = AstBlock $ Block $ scope
  where
    _fields = getBlockFields transformedChildren
    scope = Scope {fields = _fields, parentScope = Nothing}

getBlockFields :: [AstNode] -> [Field]
getBlockFields ts = _fields
  where
    localVarNodes = findDirectChildren1 isLocalVariable isBlock ts
    _fields = map (astLocalVariable . rootLabel) localVarNodes
