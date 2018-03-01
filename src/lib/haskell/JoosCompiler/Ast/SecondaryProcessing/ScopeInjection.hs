module JoosCompiler.Ast.SecondaryProcessing.ScopeInjection
  ( injectScopesIntoChildrenBlocks
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants

injectScopesIntoChildrenBlocks :: AstNode -> AstNode
injectScopesIntoChildrenBlocks _t = f Nothing _t
  where
    f :: Maybe Scope -> AstNode -> AstNode
    f _parentScope (Node (AstBlock block) children) =
      Node (AstBlock newBlock) newChildren
      where
        thisScope = blockScope block
        newScope = Scope (fields thisScope) _parentScope
        newBlock = Block newScope
        newChildren = map (f $ Just thisScope) children
    f _parentScope (Node n children) = (Node n newChildren)
      where
        newChildren = map (f _parentScope) children
