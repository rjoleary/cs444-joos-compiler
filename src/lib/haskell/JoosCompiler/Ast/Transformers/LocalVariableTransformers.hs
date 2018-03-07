module JoosCompiler.Ast.Transformers.LocalVariableTransformers
  ( localVariableTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

localVariableTransformer :: Transformer
localVariableTransformer transformedChildren t =
  AstLocalVariable $
  Local
  { localType = _type
  , localModifiers = [] -- Local variables have no modifiers
  , localName = name
  , localValue = _value
  }
  where
    _type = getType transformedChildren
    name = getVarName t
    _value = Expression _type $ Literal _type "3"
    -- TODO: _value = astExpression $ getExpression transformedChildren

getVarName :: TaggedParseTree -> String
getVarName (Node _ _children) = name
  where
    nameNode = _children !! 1
    name = head $ extractName [nameNode]
