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
checkReachability3 (Node (AstWholeProgram wp) _) =
  analyzeWholeProgram (R3 Nothing) wp

data R3 = R3 (Maybe String)

instance Analysis R3 () where
  -- Store the identifier into the context.
  analyzeStatement ctx LocalStatement{localVariable=Variable{variableName=id, variableValue=e}} =
    analyzeOuterExpression (R3 (Just id)) e
  analyzeStatement ctx x = analyzeStatement (DefaultAnalysis ctx) x

  -- Check the identifier /= the context.
  analyzeExpression (R3 (Just ctxId)) (ExpressionName [id])
    | id == ctxId = Left "Variable must not occur in its own initializer"
    | otherwise = Right ()
  analyzeExpression ctx x = analyzeExpression (DefaultAnalysis ctx) x
