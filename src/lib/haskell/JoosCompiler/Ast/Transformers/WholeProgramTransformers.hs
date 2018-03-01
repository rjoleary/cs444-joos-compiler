module JoosCompiler.Ast.Transformers.WholeProgramTransformers
  ( wholeProgramTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

wholeProgramTransformer :: [AstNode] -> AstWrapper
wholeProgramTransformer transformedChildren =
  AstWholeProgram $ WholeProgram $ map getCompilationUnit transformedChildren
  where getCompilationUnit (Node (AstCompilationUnit x) ts) = x
