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
  Field
  { fieldType = _type
  , fieldModifiers = [] -- Local variables have no modifiers
  , fieldName = name
  , fieldValue = _value
  }
  where
    _type = getType transformedChildren
    name = getVarName t
    _value = Literal "3"
    -- TODO: _value = astExpression $ getExpression transformedChildren

getVarName :: TaggedParseTree -> Name
getVarName (Node _ children) = extractName [nameNode]
  where
    nameNode = children !! 1
