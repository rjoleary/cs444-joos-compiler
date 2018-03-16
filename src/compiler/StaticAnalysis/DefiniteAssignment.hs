module StaticAnalysis.DefiniteAssignment
  ( ConstValue(..)
  , evalExpr
  ) where

import Data.Int
import JoosCompiler.Ast.NodeTypes

data ConstValue = ConstInt Int32 | ConstBool Bool | Unknown deriving (Eq, Show)

-- See JLS 16.1: Definite Assignment and Expressions
evalExpr :: Expression -> ConstValue
evalExpr Expression{ innerExpression = e } = evalInnerExpr e

-- TODO: Some of these may be incorrect (especially for integer ranges) or missing (like Assign).
binop :: BinaryOperator -> ConstValue -> ConstValue -> ConstValue
binop Multiply     (ConstInt x) (ConstInt y)   = ConstInt (x * y)
binop Divide       (ConstInt x) (ConstInt y)   = ConstInt (x `div` y)
binop Modulus      (ConstInt x) (ConstInt y)   = ConstInt (x `rem` y) -- TODO: same as java %?
binop Add          (ConstInt x) (ConstInt y)   = ConstInt (x + y)
binop Subtract     (ConstInt x) (ConstInt y)   = ConstInt (x - y)
binop Less         (ConstInt x) (ConstInt y)   = ConstBool (x < y)
binop Greater      (ConstInt x) (ConstInt y)   = ConstBool (x > y)
binop LessEqual    (ConstInt x) (ConstInt y)   = ConstBool (x <= y)
binop GreaterEqual (ConstInt x) (ConstInt y)   = ConstBool (x >= y)
binop Equality     (ConstInt x) (ConstInt y)   = ConstBool (x == y)
binop Inequality   (ConstInt x) (ConstInt y)   = ConstBool (x /= y)
binop Equality     (ConstBool x) (ConstBool y) = ConstBool (x == y)
binop Inequality   (ConstBool x) (ConstBool y) = ConstBool (x /= y)
binop LazyAnd      (ConstBool x) (ConstBool y) = ConstBool (x && y)
binop LazyOr       (ConstBool x) (ConstBool y) = ConstBool (x || y)
binop And          (ConstBool x) (ConstBool y) = ConstBool (x && y)
binop Or           (ConstBool x) (ConstBool y) = ConstBool (x || y)
binop _ _ _                                    = Unknown

unop :: UnaryOperator -> ConstValue -> ConstValue
unop Negate (ConstInt x) = ConstInt (-x)
unop Not (ConstBool x)   = ConstBool (not x)
unop _ _                 = Unknown

evalInnerExpr :: InnerExpression -> ConstValue
evalInnerExpr (BinaryOperation op e1 e2)             = binop op (evalExpr e1) (evalExpr e2)
evalInnerExpr (UnaryOperation op e)                  = unop op (evalExpr e)
evalInnerExpr (LiteralExpression (IntegerLiteral x)) = ConstInt (fromIntegral x)
evalInnerExpr (LiteralExpression (BooleanLiteral x)) = ConstBool x
evalInnerExpr _ = Unknown
