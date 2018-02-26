module JoosCompiler.Ast.Transformers.LittleTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

cstTransformer :: Transformer
cstTransformer _ (Node token _) = AstTaggedToken $ token

modifierTransformer :: Transformer
modifierTransformer _ (Node token children) = AstModifier $ modifier
  where
    leaf = getLeaf $ head children
    modifier
      | tokenString leaf == kPublic = Public
      | tokenString leaf == kProtected = Protected
      | tokenString leaf == kFinal = Final
      | tokenString leaf == kAbstract = Abstract
      | tokenString leaf == kStatic = Static
      | tokenString leaf == kNative = Native
      | otherwise = error "Invalid modifier"
