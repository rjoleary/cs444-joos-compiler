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

--typeExpression _ _ (Expression _ l@(Literal _type v)) = Expression _type l
{-typeExpression _ _ (Expression _ s@(LiteralExpression (l t)))
  | l == IntegerLiteral = Expression _typeInt s
  | l == BooleanLiteral = Expression _typeBool s
  | l == CharLiteral = Expression _typeChar s
  | l == StringLiteral = Expression _typeString s
  | l == NullLiteral = Expression Null (LiteralExpression NullLiteral)
  | otherwise = error "LiteralExpression type is wrong"
  where
    _typeInt = Type
               {innerType = Int
               ,isArray = False
               }
    _typeBool = Type
               {innerType = Boolean
               ,isArray = False
               }
    _typeChar = Type
               {innerType = Char
               ,isArray = False
               }
    _typeString = Type
               {innerType =  NamedType (["java","lang","String"])
               ,isArray = False
               }
-}
------------------------BinaryOperation-------------------------------------------------------
typeExpression cu scope (Expression _ (BinaryOperation operator e1 e2))
  | and [(expressionType typedE1 ==  expressionType typedE2),
         (innerType (expressionType typedE1) `elem` [Int, Short, Byte]),
         not((isArray (expressionType typedE1))),
         (operator `elem` [Multiply, Divide, Modulus])] =
      typedExpression
   | otherwise = error "Mutilply parameters are not the same and is invalid"
  where
    typedExpression = Expression _type (BinaryOperation operator typedE1 typedE2)
    _type = Type
            {innerType = Int
            ,isArray = False
            }
    typedE1 = typeExpression cu scope e1
    typedE2 = typeExpression cu scope e2

typeExpression cu scope (Expression _ (BinaryOperation operator e1 e2))
  | and [((innerType (expressionType typedE1) == s)
         || (innerType (expressionType typedE2) == s)),
         not ((isArray (expressionType typedE1))),
         not ((isArray (expressionType typedE2))),
         (operator == Add)] =
      typedExpression1
  | and [(innerType (expressionType typedE1) `elem` [Byte, Int, Short]),
         (innerType (expressionType typedE2) `elem` [Byte, Int, Short]),
         not ((isArray (expressionType typedE1))),
         not ((isArray (expressionType typedE2))),
         (operator `elem` [Add, Subtract])] =
      typedExpression2
  | otherwise = error " Additive operators expressions are invalid"
  where
    s = NamedType (["java","lang","String"])
    typedExpression1 = Expression _type1 (BinaryOperation operator typedE1 typedE2)
    typedExpression2 = Expression _type2 (BinaryOperation operator typedE1 typedE2)
    _type1 = Type
            {innerType = s
            ,isArray = False
            }
    _type2 = Type
            {innerType = Int
            ,isArray = False
            }
    typedE1 = typeExpression cu scope e1
    typedE2 = typeExpression cu scope e2







typeExpression cu scope (Expression _ (UnaryOperation operator e))
  | and [(operator == Not),
         not(isArray (expressionType typedE)),
         ((innerType (expressionType typedE)) `elem` [Byte, Int, Short, Char])] = typedExpression
  | and [(operator == Negate),
         not(isArray (expressionType typedE)),
         ((innerType (expressionType typedE)) == Boolean)] = typedExpression2
  | otherwise = error "wrong operand type for NOT unaryOperation"
  where
    typedExpression = Expression _type (UnaryOperation operator typedE)
    typedExpression2 = Expression _type2 (UnaryOperation operator typedE)
    _type = Type
            { innerType = Int
            , isArray = False
            }
    _type2 = expressionType e
    typedE = typeExpression cu scope e




------------          ExpressionName ------------------------
typeExpression cu scope(Expression t (ExpressionName n))
---  | (&&) (not (isArray t) )  (na== n) = Expression localType(l) (ExpressionName n)
  | (&&) (not (isArray t) ) (unNamedType (innerType t) == n) = Expression t (ExpressionName n)
  | otherwise = error "name expression is invalid"
 -- where
 --  na = unNamedType(innerType (t)) --Name
 --  l = resolveInScope scope (head na)
   

















--resolveInScope :: Scope -> String -> Local
--resolveInScope (Scope locals Nothing) "string" =  Local
  --    { localType = Type
    --  , localModifiers = []
     -- , localName = Name
     -- , localValue = Expression Type (Literal Type "3")
     -- }
      
