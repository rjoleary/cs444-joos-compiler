module JoosCompiler.Ast.Transformers.TypeTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

typeTransformer :: Transformer
typeTransformer transformedChildren t =
  AstType $ Type {joosType = innerType, isArray = _isArray}
  where
    _isArray = ((> 0) . length) $ findChildrenByTokenName kArrayType t
    typeNameLeaf = head $ getLeaves t
    typeString = tokenString typeNameLeaf
    innerType
      | typeString == kBoolean = Boolean
      | typeString == kByte = Byte
      | typeString == kChar = Char
      | typeString == kInt = Int
      | typeString == kShort = Short
      | otherwise = NamedType typeString

getFieldName :: TaggedParseTree -> String
getFieldName t = tokenString $ rootLabel identifierNode
  where
    variableDeclarator = head $ findChildrenByTokenName kVariableDeclarator t
    identifierNode =
      head $
      findDirectChildrenByTokenName kIdentifier kExpression variableDeclarator
