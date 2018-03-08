module JoosCompiler.Ast.Transformers.FieldTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

fieldTransformer :: Transformer
fieldTransformer transformedChildren t@(Node _ (modifiers:myType:_)) =
  AstField $
  Field
  { fieldType = _type
  , fieldModifiers = _fieldModifiers
  , fieldName = name
  , fieldValue = _value
  }
  where
    _type = typeTransformer myType
    _fieldModifiers = astModifiers $ getModifiers transformedChildren
    name = getFieldName t
    _value = Expression _type $ LiteralExpression $ StringLiteral "TODO"
    -- TODO: _value = astExpression $ getExpression transformedChildren

getFieldName :: TaggedParseTree -> Name
getFieldName t = map (tokenString . rootLabel) identifierNodes
  where
    variableDeclarator = head $ findChildrenByTokenName kVariableDeclarator t
    identifierNodes =
      findDirectChildrenByTokenName kIdentifier kExpression variableDeclarator
