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
checkTypes ast@(Node (AstWholeProgram program) children) = do

  ruleFor expressions "Binary operation error" $
    (applyFnToScopedExpression program getExpressionType)

  where
    ruleFor nodes err f = asEither $ filter (not . f) $ nodes
      where asEither (x:_) = Left ("Error: " ++ err)
            asEither []    = Right ()

    expressions = findScopedExpressions ast

findScopedExpressions :: AstNode -> [(Scope, Expression)]
findScopedExpressions t = f (Scope [] Nothing []) t
  where
    f :: Scope -> AstNode -> [(Scope, Expression)]
    f scope (Node (AstExpression e) children) = (scope, e) : (mconcat $ map (f scope) children)
    f _ (Node (AstBlock (Block scope)) children) = mconcat $ map (f scope) children
    f scope (Node _ children) = mconcat $ map (f scope) children

isBinaryOperation :: Expression -> Bool
isBinaryOperation (Expression _ (BinaryOperation _ _ _)) = True
isBinaryOperation _ = False

applyFnToScopedExpression ::
  WholeProgram -> (WholeProgram -> Scope -> Expression -> Either a b) -> (Scope, Expression) -> Bool
applyFnToScopedExpression p f (s, e) = case (f p s e) of
  Left _ -> False
  Right _ -> True
