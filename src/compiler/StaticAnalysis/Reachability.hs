{-# LANGUAGE MultiParamTypeClasses #-}
module StaticAnalysis.Reachability
  ( checkReachability ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Tree
import JoosCompiler.Ast
import JoosCompiler.Ast.ConstantExpression
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Visitor.Analysis
import JoosCompiler.Error
import JoosCompiler.TreeUtils

data Status = CompletesNormally | CompletesAbnormally deriving (Eq, Show)

instance Monoid Status where
  mempty = CompletesNormally
  mappend CompletesAbnormally _ = CompletesAbnormally
  mappend _ CompletesAbnormally = CompletesAbnormally
  mappend _ _                   = CompletesNormally

checkReachability :: AstNode -> Either String ()
checkReachability (Node x _) =
  fmap (const ()) $ (analyze CheckReachability x :: Either String Status)

data CheckReachability = CheckReachability

instance Analysis CheckReachability Status where
  analyze ctx (AstMethod m@Method{methodReturn=r, methodStatement=s}) =
    if isMethodAbstract m || isMethodNative m
      then Right CompletesNormally -- Ignore methods with not implementation.
      else do
        status <- analyze' ctx s
        if r /= Void && status == CompletesNormally
          then Left ("Non-void method '" ++ methodName m ++ "' must return on all paths")
          else return status

  analyze ctx (AstStatement s@LoopStatement{loopPredicate=e, nextStatement=n})
    | evalExpr e == ConstBool True && n /= TerminalStatement =
      Left "Statements may not proceed a loop which does not complete normally"
    | evalExpr e == ConstBool False =
      Left "A loop condition must not always evaluate to false"
    | evalExpr e == ConstBool True =
      Right CompletesAbnormally
    | otherwise = analyze' ctx n

  analyze ctx (AstStatement s@IfStatement{}) = do
    ifThenStatus <- analyze' ctx $ ifThenStatement s
    ifElseStatus <- analyze' ctx $ ifElseStatement s
    nextStatus <- analyze' ctx $ nextStatement s
    if ifThenStatus == CompletesAbnormally && ifElseStatus == CompletesAbnormally
    then
      if nextStatement s == TerminalStatement
      then return CompletesAbnormally
      else Left "Statements may not proceed an if-statement which does not complete normally"
    else return nextStatus

  -- No statement is allowed after the return statement.
  analyze ctx (AstStatement ReturnStatement{nextStatement=TerminalStatement}) =
    Right CompletesAbnormally
  analyze ctx (AstStatement ReturnStatement{nextStatement=_}) =
    Left "No statement allowed after the return statement"

  -- Everything else propagates.
  analyze ctx x = propagateAnalyze ctx x
