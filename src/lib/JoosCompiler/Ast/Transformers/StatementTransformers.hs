module JoosCompiler.Ast.Transformers.StatementTransformers
  ( statementTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

statementTransformer :: Transformer
statementTransformer transformedChildren t =
  AstStatement $ Statement {statement = EmptyStatement}
  where
    scope = createScope transformedChildren

createScope :: [AstNode] -> Scope
createScope ts = Scope vars Nothing []
  where
    varNodes = findDirectChildren1 isLocalVariable isBlock ts
    vars = map (astLocalVariable . rootLabel) varNodes
