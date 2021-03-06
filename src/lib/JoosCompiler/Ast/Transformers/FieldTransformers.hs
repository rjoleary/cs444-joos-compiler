module JoosCompiler.Ast.Transformers.FieldTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

fieldTransformer :: Transformer
fieldTransformer transformedChildren t@(Node _ (modifiers:myType:varDeclarator:_)) =
  AstField $
  Variable
  { variableType = _type
  , variableModifiers = _fieldModifiers
  , variableName = name
  , variableValue = _value
  -- TODO(Ahmed) error "variableCanonicalName not ready (Field)"
  , variableCanonicalName = [name]
  }
  where
    _type = typeTransformer myType
    _fieldModifiers = astModifiers $ getModifiers transformedChildren
    name = getFieldName t
    _value = getFieldExpression varDeclarator _type

getFieldName :: TaggedParseTree -> String
getFieldName t
  | length fieldNameList > 1 = "Field name contains too many parts"
  | otherwise = head fieldNameList
  where
    fieldNameList = map (tokenString . rootLabel) identifierNodes
    variableDeclarator = head $ findChildrenByTokenName kVariableDeclarator t
    identifierNodes =
      findDirectChildrenByTokenName kIdentifier kExpression variableDeclarator

getFieldExpression :: TaggedParseTree -> Type -> Expression
getFieldExpression (Node _ [(Node (TaggedToken "Identifier" _ _ _) []), (Node (TaggedToken "=" _ _ _) []), e]) _ =
  expressionTransformer e
getFieldExpression (Node _ [(Node (TaggedToken "Identifier" _ _ _) [])]) t =
  uninitializedExpression t
