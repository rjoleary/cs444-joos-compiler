module JoosCompiler.Ast.Transformers.FieldTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

fieldTransformer :: Transformer
fieldTransformer transformedChildren t =
  AstField $
  Field
  { fieldType = _type
  , fieldModifiers = _fieldModifiers
  , fieldName = name
  , fieldValue = _value
  , isClassField = True
  }
  where
    _type = getType transformedChildren
    _fieldModifiers = astModifiers $ getModifiers transformedChildren
    name = getFieldName t
    _value = Literal "3"
    -- TODO: _value = astExpression $ getExpression transformedChildren

getFieldName :: TaggedParseTree -> Name
getFieldName t = map (tokenString . rootLabel) identifierNodes
  where
    variableDeclarator = head $ findChildrenByTokenName kVariableDeclarator t
    identifierNodes =
      findDirectChildrenByTokenName kIdentifier kExpression variableDeclarator
