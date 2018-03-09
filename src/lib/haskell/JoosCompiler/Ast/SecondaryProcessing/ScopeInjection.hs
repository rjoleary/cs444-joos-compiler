module JoosCompiler.Ast.SecondaryProcessing.ScopeInjection
  ( injectScopesIntoChildrenBlocks
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants

injectScopesIntoChildrenBlocks :: AstNode -> AstNode
injectScopesIntoChildrenBlocks _t = f [] Nothing _t
  where
    f :: Name -> Maybe Scope -> AstNode -> AstNode
    f cuName _parentScope (Node unitNode@(AstCompilationUnit (CompilationUnit _ _ _ _cuName)) _children) =
      Node unitNode $ map (f cuName _parentScope) _children
    f cuName _parentScope (Node (AstBlock block) _children) =
      Node (AstBlock newBlock) newChildren
      where
        thisScope = blockScope block
        newScope = Scope (scopeLocals thisScope) _parentScope cuName
        newBlock = Block newScope
        newChildren = map (f cuName $ Just thisScope) _children
    f cuName _parentScope (Node n _children) = (Node n newChildren)
      where
        newChildren = map (f cuName _parentScope) _children
