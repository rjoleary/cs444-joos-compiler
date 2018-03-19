{-# LANGUAGE MultiParamTypeClasses #-}
module StaticAnalysis.Reachability3 (checkReachability3) where

import Data.Tree
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Transformers.Types
import JoosCompiler.Ast.Visitor.Analysis

-- This checks the 3rd rule of reachability: Every local variable must have an
-- initializer, and the variable must not occur in its own initializer

checkReachability3 :: AstNode -> Either String ()
checkReachability3 (Node x _) = analyze (R3 Nothing) x

data R3 = R3 (Maybe String)

instance Analysis R3 () where
  -- Store the identifier into the context.
  analyze ctx (AstStatement LocalStatement{localVariable=Variable{variableName=id, variableValue=e}}) =
    analyze' (R3 (Just id)) e

  -- Check the identifier /= the context.
  analyze (R3 (Just ctxId)) (AstExpression (Expression _ (ExpressionName [id])))
    | id == ctxId = Left "Variable must not occur in its own initializer"
    | otherwise = Right ()

  -- Everything else propagates.
  analyze ctx x = propagateAnalyze ctx x
