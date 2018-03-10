module Linking.TypeChecking
  ( checkTypes
  ) where

import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Transformers.Types
import JoosCompiler.Ast.Utils
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
    checkStatementType (AstStatement (Statement s@ExpressionStatement{})) = do
      exprType <- getExprType' $ statementExpression s
      return ()

    -- LoopStatement
    checkStatementType (AstStatement (Statement LoopStatement{})) = do
      return ()

    -- IfStatement
    checkStatementType (AstStatement (Statement IfStatement{})) = do
      return ()

    -- ReturnStatement
    checkStatementType (AstStatement (Statement ReturnStatement{})) = do
      return ()

    -- LocalStatement
    checkStatementType (AstStatement (Statement LocalStatement{})) = do
      return ()

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
