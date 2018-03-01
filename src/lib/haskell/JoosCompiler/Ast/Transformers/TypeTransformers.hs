module JoosCompiler.Ast.Transformers.TypeTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

typeTransformer :: Transformer
typeTransformer transformedChildren t = AstType $ _type
  where
    _isArray = ((> 0) . length) $ findChildrenByTokenName kArrayType t
    isPrimitiveType = (> 0) $ length $ findChildrenByTokenName kPrimitiveType t
    primitiveType = getPrimitiveType t _isArray
    complexType = getComplexType t _isArray
    _type
      | isPrimitiveType = primitiveType
      | otherwise = complexType

getComplexType :: TaggedParseTree -> Bool -> Type
getComplexType t _isArray
  | isVoid = Void
  | otherwise = Type {innerType = _innerType, isArray = _isArray}
  where
    nameNodes = findChildrenByTokenName kIdentifier t
    typeName = map (tokenString . rootLabel) nameNodes
    isVoid = typeName == [kVoid] || typeName == []
    _innerType = NamedType typeName

getPrimitiveType :: TaggedParseTree -> Bool -> Type
getPrimitiveType t _isArray = Type {innerType = _innerType, isArray = _isArray}
  where
    typeName = tokenString $ head $ getLeaves t
    _innerType
      | typeName == kBoolean = Boolean
      | typeName == kByte = Byte
      | typeName == kChar = Char
      | typeName == kInt = Int
      | typeName == kShort = Short
      | otherwise = error "Unexpected type name"
