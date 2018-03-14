module JoosCompiler.Ast.SecondaryProcessing.StatementBlocks
  (
    insertBlocksAroundStatements
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TreeUtils

insertBlocksAroundStatements :: AstNode -> AstNode
insertBlocksAroundStatements t = f t
  where
    f :: AstNode -> AstNode
    f t@(Node (AstLocalVariable l) _children) =
      -- Partially empty scope. Will be filled later.
      Node (AstBlock (Block $ Scope [l] Nothing [])) [newNode]
      where
        newNode = (Node (AstLocalVariable l) $ map f _children)
    f (Node root _children) = (Node root $ map f _children)
