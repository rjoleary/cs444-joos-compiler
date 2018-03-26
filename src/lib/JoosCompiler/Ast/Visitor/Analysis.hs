{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module JoosCompiler.Ast.Visitor.Analysis
  ( Analysis(..)
  , propagateAnalyze
  ) where

import Data.Maybe
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Transformers.Types
import JoosCompiler.Error

-- Analysis takes the entire tree and outputs a single value.
class Analysis a b where
  analyze :: a -> AstWrapper -> Either String b
  analyze' :: (AstWrappable c) => a -> c -> Either String b
  analyze' ctx x = analyze ctx (wrap x)

-- This analysis will call all child nodes and perform the given analysis.
-- Return values are concatenated as a monoid.
data PropagateAnalysis a = PropagateAnalysis a

propagateAnalyze ctx x = analyze (PropagateAnalysis ctx) x

instance (Analysis c b, Monoid b) => Analysis (PropagateAnalysis c) b where
  analyze (PropagateAnalysis ctx) (AstTypeDeclaration x) =
    concatEithers $
      fmap (analyze' ctx) (classFields x) ++
      fmap (analyze' ctx) (methods x) ++
      fmap (analyze' ctx) (constructors x)

  analyze (PropagateAnalysis ctx) (AstWholeProgram x) =
    concatEithers $
      fmap (analyze' ctx) (programCus x)

  analyze (PropagateAnalysis ctx) (AstCompilationUnit x) =
    concatEithers $
      fmap (analyze' ctx) (imports x) ++
      fmap (analyze' ctx) (maybeToList (typeDecl x))

  -- TODO: remove the outer node
  analyze (PropagateAnalysis ctx) (AstExpression (MethodInvocation x _ xs)) =
    concatEithers $ fmap (analyze' ctx) (x:xs)
  analyze (PropagateAnalysis ctx) (AstExpression (BinaryOperation _ x y)) =
    concatEithers $ fmap (analyze' ctx) [x, y]
  analyze (PropagateAnalysis ctx) (AstExpression (UnaryOperation _ x)) =
    analyze' ctx x
  analyze (PropagateAnalysis ctx) (AstExpression (LiteralExpression _)) =
    Right mempty
  analyze (PropagateAnalysis ctx) (AstExpression This) =
    Right mempty
  analyze (PropagateAnalysis ctx) (AstExpression (DynamicFieldAccess e _)) =
    concatEithers $ fmap (analyze' ctx) [e]
  analyze (PropagateAnalysis ctx) (AstExpression (ExpressionName name)) =
    Right mempty
  analyze (PropagateAnalysis ctx) (AstExpression (NewExpression name es)) =
    concatEithers $ fmap (analyze' ctx) es
  analyze (PropagateAnalysis ctx) (AstExpression (NewArrayExpression name arg)) =
    analyze' ctx arg
  analyze (PropagateAnalysis ctx) (AstExpression (CastExpression t e)) =
    concatEithers $ [analyze' ctx t, analyze' ctx e]
  analyze (PropagateAnalysis ctx) (AstExpression (InstanceOfExpression e t)) =
    concatEithers $ [analyze' ctx e, analyze' ctx t]
  analyze (PropagateAnalysis ctx) (AstExpression (ArrayExpression e1 e2)) =
    concatEithers $ [analyze' ctx e1, analyze' ctx e2]

  analyze (PropagateAnalysis ctx) (AstField x) =
    concatEithers $
      [analyze' ctx (variableType x)] ++
      [analyze' ctx (variableValue x)]

  analyze (PropagateAnalysis ctx) (AstImport x) =
    Right mempty

  analyze (PropagateAnalysis ctx) (AstMethod x) =
    concatEithers $
      fmap (analyze' ctx) (methodParameters x) ++
      [analyze' ctx (methodStatement x)]

  analyze (PropagateAnalysis ctx) (AstStatement x@BlockStatement{}) =
    concatEithers $
      [ analyze' ctx (statementBlock x)
      , analyze' ctx (nextStatement x) ]
  analyze (PropagateAnalysis ctx) (AstStatement x@ExpressionStatement{}) =
    concatEithers $
      [ analyze' ctx (statementExpression x)
      , analyze' ctx (nextStatement x) ]
  analyze (PropagateAnalysis ctx) (AstStatement x@LoopStatement{}) =
    concatEithers $
      [ analyze' ctx (loopPredicate x)
      , analyze' ctx (loopStatement x)
      , analyze' ctx (nextStatement x) ]
  analyze (PropagateAnalysis ctx) (AstStatement x@IfStatement{}) =
    concatEithers $
      [ analyze' ctx (ifPredicate x)
      , analyze' ctx (ifThenStatement x)
      , analyze' ctx (ifElseStatement x)
      , analyze' ctx (nextStatement x) ]
  analyze (PropagateAnalysis ctx) (AstStatement x@ReturnStatement{}) =
    concatEithers $
      fmap (analyze' ctx) (maybeToList $ returnExpression x) ++
      [ analyze' ctx (nextStatement x) ]
  analyze (PropagateAnalysis ctx) (AstStatement x@LocalStatement{}) =
    concatEithers $
      [ analyze' ctx (localVariable x)
      , analyze' ctx (nextStatement x) ]
  analyze (PropagateAnalysis ctx) (AstStatement x@EmptyStatement{}) =
    analyze' ctx (nextStatement x)
  analyze (PropagateAnalysis ctx) (AstStatement TerminalStatement) =
    Right mempty

  analyze (PropagateAnalysis ctx) (AstType x) =
    Right mempty
