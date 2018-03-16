module StaticAnalysis.DefiniteAssignment
  ( ConstantValue
  , evalExpr
  ) where

import JoosCompiler.Ast.NodeTypes

type ConstantValue = Maybe Bool

-- See JLS 16.1: Definite Assignment and Expressions
evalExpr :: Expression -> ConstantValue
evalExpr Expression{ innerExpression = e } = evalInnerExpr e

evalInnerExpr :: InnerExpression -> ConstantValue
evalInnerExpr (BinaryOperation And e1 e2)            = (&&) <$> evalExpr e1 <*> evalExpr e2
evalInnerExpr (BinaryOperation Or e1 e2)             = (||) <$> evalExpr e1 <*> evalExpr e2
evalInnerExpr (UnaryOperation Not e)                 = (not) <$> evalExpr e
evalInnerExpr (LiteralExpression (BooleanLiteral b)) = Just b
evalInnerExpr _ = Nothing
