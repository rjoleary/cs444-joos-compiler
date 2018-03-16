module Linking.TypeChecking
  ( checkTypes
  ) where

import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import Control.Monad
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Transformers.Types
import JoosCompiler.Ast.Utils
import JoosCompiler.Error
import JoosCompiler.TreeUtils
import Linking.TypeChecking.BinaryOperation
import Types

-- Returns a error message if hierarchy checking fails.
checkTypes :: AstNode -> Either String ()
checkTypes ast@(Node (AstWholeProgram program) children) =
  (foldEither $ flatten $ fmap checkStatementType $ stdFilter ast) >> return ()

  where
    stdFilter (Node x@(AstCompilationUnit (CompilationUnit{cuPackage=("java":_)})) _) = Node x []
    stdFilter (Node x xs) = Node x (map stdFilter xs)

    expressions = findScopedExpressions ast

    getScope :: Expression -> Scope
    getScope expr = fst $ head $ filter (\(_, e) -> e == expr) expressions

    -- Short form
    getExprType' e = getExpressionType program (getScope e) e

    checkStatementType :: AstWrapper -> Either String ()

    -- ExpressionStatement
    checkStatementType (AstStatement s@ExpressionStatement{}) = do
      exprType <- getExprType' $ statementExpression s
      return ()

    -- LoopStatement
    checkStatementType (AstStatement s@LoopStatement{}) = do
      predicateType <- getExprType' $ loopPredicate s
      when (not $ isBoolean $ predicateType)
        (Left "Loop predicate must be a boolean")

    -- IfStatement
    checkStatementType (AstStatement s@IfStatement{}) = do
      predicateType <- getExprType' $ ifPredicate s
      when (not $ isBoolean $ predicateType)
        (Left "If predicate must be a boolean")

    -- ReturnStatement
    checkStatementType (AstStatement ReturnStatement{returnExpression=Just e}) = do
      returnType <- getExprType' $ e
      when (returnType == Void) (Left "Cannot return void")

    -- LocalStatement
    checkStatementType (AstStatement LocalStatement{localVariable=l}) = do
      exprType <- getExprType' $ variableValue l
      when (exprType /= variableType l) (Left "Local statement type doesn't match")

    checkStatementType _ = return ()

findScopedExpressions :: AstNode -> [(Scope, Expression)]
findScopedExpressions t = f (Scope [] Nothing []) t
  where
    f :: Scope -> AstNode -> [(Scope, Expression)]
    f scope (Node (AstCompilationUnit unit) children)
      | length pName > 0 && head pName == "java" = []
      | otherwise = mconcat $ map (f scope) children
      where
        pName = cuPackage unit
    f scope (Node (AstExpression e) children) = (scope, e) : (mconcat $ map (f scope) children)
    f _ (Node (AstBlock (Block scope)) children) = mconcat $ map (f scope) children
    f scope (Node _ children) = mconcat $ map (f scope) children

isBinaryOperation :: Expression -> Bool
isBinaryOperation (Expression _ (BinaryOperation _ _ _)) = True
isBinaryOperation _ = False
