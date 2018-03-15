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
localVariableTransformer transformedChildren t@(Node _ [myType,identifier,_,expression]) =
  AstLocalVariable $
  Local
  { localType = _type
  , localModifiers = [] -- Local variables have no modifiers
  , localName = name
  , localValue = _value
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
