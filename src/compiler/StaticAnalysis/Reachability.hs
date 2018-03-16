{-# LANGUAGE MultiParamTypeClasses #-}
module StaticAnalysis.Reachability
  ( checkReachability ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Tree
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Visitor.Analysis
import JoosCompiler.Error
import JoosCompiler.TreeUtils
import StaticAnalysis.DefiniteAssignment

data Status = CompletesNormally | CompletesAbnormally deriving (Eq, Show)

instance Monoid Status where
  mempty = CompletesNormally
  mappend CompletesAbnormally _ = CompletesAbnormally
  mappend _ CompletesAbnormally = CompletesAbnormally
  mappend _ _                   = CompletesNormally

checkReachability :: AstNode -> Either String ()
checkReachability (Node (AstWholeProgram wp) _) =
  fmap (const ()) $ (analyzeWholeProgram CheckReachability wp :: Either String Status)

data CheckReachability = CheckReachability

instance Analysis CheckReachability Status where
  analyzeMethod ctx m@Method{methodReturn=r, methodStatement=s} =
    if s == TerminalStatement
      then Right CompletesNormally -- Ignore methods with not implementation.
      else do
        status <- analyzeStatement ctx s
        if r /= Void && status == CompletesNormally
          then Left ("Non-void method '" ++ methodName m ++ "' must return on all paths")
          else return status

  -- A while loop condition must not evaluate to true.
  analyzeStatement ctx s@LoopStatement{loopPredicate=e, nextStatement=n}
    | evalExpr e == ConstBool True && n /= TerminalStatement =
      Left "Statements may not proceed a loop which does not complete normally"
    | evalExpr e == ConstBool False =
      Left "A loop condition must not always evaluate to false"
    | evalExpr e == ConstBool True =
      Right CompletesAbnormally
    | otherwise = analyzeStatement ctx n

  analyzeStatement ctx s@IfStatement{} = do
    ifThenStatus <- analyzeStatement ctx $ ifThenStatement s
    ifElseStatus <- analyzeStatement ctx $ ifElseStatement s
    nextStatus <- analyzeStatement ctx $ nextStatement s
    if ifThenStatus == CompletesAbnormally && ifElseStatus == CompletesAbnormally
    then
      if nextStatement s == TerminalStatement
      then return CompletesAbnormally
      else Left "Statements may not proceed an if-statement which does not complete normally"
    else return nextStatus

  -- No statement is allowed after the return statement.
  analyzeStatement ctx ReturnStatement{nextStatement=TerminalStatement} =
    Right CompletesAbnormally
  analyzeStatement ctx ReturnStatement{nextStatement=_} =
    Left "No statement allowed after the return statement"

  -- All other statements complete normally.
  analyzeStatement ctx s = analyzeStatement (DefaultAnalysis ctx) s
