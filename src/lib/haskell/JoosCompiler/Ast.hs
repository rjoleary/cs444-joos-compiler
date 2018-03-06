module JoosCompiler.Ast
  ( cstsToAst
  , AstWrapper(..)
  , AstNode
  , isBlock
  , isTypeDeclaration
  , isField
  , isImport
  , isLocalVariable
  , isMethod
  , isModifier
  , isModifiers
  , isPackage
  , isType
  ) where

import           JoosCompiler.Ast.Core
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Treeify
