module JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
  ( canonicalizeTypes
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types

canonicalizeTypes :: AstNode -> AstNode
canonicalizeTypes t = t
