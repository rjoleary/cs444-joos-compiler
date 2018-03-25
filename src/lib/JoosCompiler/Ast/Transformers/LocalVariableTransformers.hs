module JoosCompiler.Ast.Transformers.LocalVariableTransformers
  ( localVariableTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

localVariableTransformer :: Transformer
localVariableTransformer _ t@(Node _ [myType,identifier,_,expression]) =
  AstLocalVariable $
  Variable
  { variableType = _type
  , variableModifiers = [] -- Local variables have no modifiers
  , variableName = name
  , variableValue = _value
  , variableCanonicalName = error "variableCanonicalName not valid for locals"
  }
  where
    _type = typeTransformer myType
    name = getVarName t
    _value = expressionTransformer expression

getVarName :: TaggedParseTree -> String
getVarName (Node _ _children) = name
  where
    nameNode = _children !! 1
    name = head $ extractName [nameNode]
