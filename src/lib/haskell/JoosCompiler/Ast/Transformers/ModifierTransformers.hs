module JoosCompiler.Ast.Transformers.ModifierTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

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

modifiersTransformer :: Transformer
modifiersTransformer transformedChildren _ = AstModifiers $ modifiers
  where
    modifierNodes = mconcat $ map (findChildren isModifier) transformedChildren
    modifiers = map (astModifier . rootLabel) modifierNodes
