module JoosCompiler.Ast.SecondaryProcessing.ExpressionTyping
  ( typeAstExpressions
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

typeAstExpressions :: AstNode -> AstNode
typeAstExpressions _t = addTypes Nothing _t

addTypes :: Maybe Scope -> AstNode -> AstNode
addTypes _ (Node (AstBlock block) _children) =
  Node (AstBlock block) typedChildren
  where
    scope = blockScope block
    typedChildren = map (addTypes $ Just scope) _children

addTypes scope (Node (AstExpression (Expression _ (MethodInvocation name arguments))) _children)
  | (map localType $ methodParameters method) == map calculateType arguments = typedExpression
  | otherwise = error "Method signature does not match argument list types"
  where
    method = resolveMethod name
    typedExpression = Node (AstExpression $ Expression _type (MethodInvocation name arguments)) newChildren
    newChildren = map (addTypes scope) _children
    _type = methodReturn method

addTypes scope (Node (AstExpression (Expression _ l@(Literal _type v))) _children) =
  Node (AstExpression $ Expression _type l) newChildren
  where
    newChildren = map (addTypes scope) _children

addTypes scope (Node n _children) = (Node n newChildren)
  where
    newChildren = map (addTypes scope) _children

-- TODO
calculateType :: Expression -> Type
calculateType _ = Type Int False
