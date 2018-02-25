module JoosCompiler.Ast.Transformers.Types where

import Data.Tree
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Treeify

data AstWrapper = AstClassDeclaration ClassDeclaration
  | AstCompilationUnit CompilationUnit
  | AstModifier Modifier
  | AstScope Scope

type AstNode = Tree AstWrapper
type Transformer = [AstNode] -> TaggedParseTree -> AstWrapper
