module AstRule where

import           JoosCompiler.Ast
import           JoosCompiler.Ast.NodeTypes

type AstRulePredicate = AstNode -> Bool

type AstRule = (String, AstRulePredicate)
