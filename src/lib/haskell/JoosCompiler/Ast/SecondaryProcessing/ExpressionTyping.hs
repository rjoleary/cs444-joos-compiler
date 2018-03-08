module JoosCompiler.Ast.SecondaryProcessing.ExpressionTyping
  ( typeAstExpressions
  ) where

import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

typeAstExpressions :: AstNode -> AstNode
typeAstExpressions n = typeAstExpressionsInner Nothing Nothing n

typeAstExpressionsInner :: Maybe CompilationUnit -> Maybe Scope -> AstNode -> AstNode
typeAstExpressionsInner _ _ (Node (AstCompilationUnit unit) _children) =
  Node (AstCompilationUnit unit) typedChildren
  where
    typedChildren = map (typeAstExpressionsInner (Just unit) $ Nothing) _children

typeAstExpressionsInner cu _ (Node (AstBlock block) _children) =
  Node (AstBlock block) typedChildren
  where
    scope = blockScope block
    typedChildren = map (typeAstExpressionsInner cu $ Just scope) _children

typeAstExpressionsInner cu scope (Node (AstExpression e) _children) =
  Node (AstExpression $ typedExpression) newChildren
  where
    newChildren = map (typeAstExpressionsInner cu scope) _children
    typedExpression = typeExpression (fromJust cu) scope e


typeExpression :: CompilationUnit -> Maybe Scope -> Expression -> Expression
typeExpression cu scope (Expression _ (MethodInvocation parentObjectName methodName arguments))
  | (map localType $ methodParameters method) == (map expressionType typedArguments) = typedExpression
  | otherwise = error "Method signature does not match argument list types"
  where
    method = resolveMethod ["TODO"]
    typedArguments = map (typeExpression cu scope) arguments
    typedExpression = Expression _type (MethodInvocation parentObjectName methodName typedArguments)
    _type = methodReturn method

typeExpression _ _ (Expression _ l@(Literal _type v)) = Expression _type l

------------------------BinaryOperation-------------------------------------------------------
typeExpression cu scope (Expression _ (BinaryOperation operator e1 e2))
  | and [(expressionType typedE1 ==  expressionType typedE2),
         (innerType (expressionType typedE1) == Int),
         not((isArray (expressionType typedE1))),
         (operator `elem` [Multiply, Divide, Modulus])] =
      typedExpression
  | and [(expressionType typedE1 ==  expressionType typedE2),
         (innerType (expressionType typedE1) == Short),
         not((isArray (expressionType typedE1)))] =
      typedExpression
  | otherwise = error "Mutilply parameters are not the same and is invalid"
  where
    typedExpression = Expression _type (BinaryOperation Multiply typedE1 typedE2)
    _type = expressionType e1
    typedE1 = typeExpression cu scope e1
    typedE2= typeExpression cu scope e2




------------          ExpressionName ------------------------
typeExpression cu scope(Expression t (ExpressionName s))
  | (&&) (not (isArray t) ) (unNamedType (innerType t) == s) = Expression t (ExpressionName s)
  | otherwise = error "name expression is invalid"
