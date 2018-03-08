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


------------------------BinaryOperation-------------------------------------------------------
addTypes scope (Node (AstExpression (Expression _ (BinaryOperation operator e1 e2))) _children)
  | and [(expressionType typedE1 ==  expressionType typedE2), (innerType (expressionType typedE1) == Int), not((isArray (expressionType typedE1))), (operator `elem` [Multiply, Divide, Modulus])]= typedExpression
  | and [(expressionType typedE1 ==  expressionType typedE2), (innerType (expressionType typedE1) == Short), not((isArray (expressionType typedE1)))]= typedExpression
  | otherwise = error "Mutilply parameters are not the same and is invalid"
  where
    typedExpression = Node (AstExpression $ Expression _type (BinaryOperation Multiply typedE1 typedE2)) newChildren
    newChildren = map (addTypes scope) _children
    _type = expressionType e1
    typedE1 = e1 -- addTypes scope e1
    typedE2= e2 -- addTypes scope e2 

addTypes scope (Node n _children) = (Node n newChildren)
  where
    newChildren = map (addTypes scope) _children



-- TODO
calculateType :: Expression -> Type
calculateType _ = Type Int False
