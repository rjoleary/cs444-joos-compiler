module JoosCompiler.Ast.Transformers.BlockTransformers
  ( blockTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TreeUtils

blockTransformer :: Transformer
blockTransformer transformedChildren t = AstBlock $ Block $ _scope
  where
    _locals = getBlockLocals transformedChildren
    _scope = Scope {scopeLocals = _locals, parentScope = Nothing}

getBlockLocals :: [AstNode] -> [Local]
getBlockLocals ts = _locals
  where
    localVarNodes = findDirectChildren1 isLocalVariable isBlock ts
    _locals = map (astLocalVariable . rootLabel) localVarNodes
