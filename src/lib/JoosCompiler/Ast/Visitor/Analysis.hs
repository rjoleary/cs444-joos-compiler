{-# LANGUAGE MultiParamTypeClasses #-}
module JoosCompiler.Ast.Visitor.Analysis
  (
  Analysis(..)
  ) where

import Data.Maybe
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Error

concatEithers :: Monoid a => [Either String a] -> Either String a
concatEithers = fmap mconcat . foldEither

-- Analysis takes the entire tree and outputs a single value.
class Analysis a b where
  analyzeTypeDeclaration :: Monoid b => a -> TypeDeclaration -> Either String b
  analyzeTypeDeclaration ctx x =
    concatEithers $
      fmap (analyzeVariable ctx) (classFields x) ++
      fmap (analyzeMethod ctx) (methods x) ++
      fmap (analyzeMethod ctx) (constructors x)

  analyzeWholeProgram :: Monoid b => a -> WholeProgram -> Either String b
  analyzeWholeProgram ctx x =
    concatEithers $
      fmap (analyzeCompilationUnit ctx) (programCus x)

  analyzeCompilationUnit :: Monoid b => a -> CompilationUnit -> Either String b
  analyzeCompilationUnit ctx x =
    concatEithers $
      fmap (analyzeImport ctx) (imports x) ++
      fmap (analyzeTypeDeclaration ctx) (maybeToList (typeDecl x))

  -- TODO: remove this node
  analyzeOuterExpression :: Monoid b => a -> Expression -> Either String b
  analyzeOuterExpression ctx (Expression _ x) = analyzeExpression ctx x

  analyzeExpression :: Monoid b => a -> InnerExpression -> Either String b
  analyzeExpression ctx (MethodInvocation x _ xs) =
    concatEithers $ fmap (analyzeOuterExpression ctx) (x:xs)
  analyzeExpression ctx (BinaryOperation _ x y) =
    concatEithers $ fmap (analyzeOuterExpression ctx) [x, y]
  analyzeExpression ctx (UnaryOperation _ x) =
    analyzeOuterExpression ctx x
  analyzeExpression ctx (LiteralExpression _) =
    Right mempty
  analyzeExpression ctx This =
    Right mempty
  analyzeExpression ctx (FieldAccess e _) =
    concatEithers $ fmap (analyzeOuterExpression ctx) [e]
  analyzeExpression ctx (ExpressionName name) =
    Right mempty
  analyzeExpression ctx (NewExpression name es) =
    concatEithers $ fmap (analyzeExpression ctx . innerExpression) es
  analyzeExpression ctx (NewArrayExpression name arg) =
    analyzeOuterExpression ctx arg
  analyzeExpression ctx (CastExpression t e) =
    concatEithers $ [analyzeType ctx t, analyzeOuterExpression ctx e]
  analyzeExpression ctx (InstanceOfExpression e t) =
    concatEithers $ [analyzeOuterExpression ctx e, analyzeType ctx t]
  analyzeExpression ctx (ArrayExpression e1 e2) =
    concatEithers $ [analyzeOuterExpression ctx e1, analyzeOuterExpression ctx e2]

  analyzeVariable :: Monoid b => a -> Variable -> Either String b
  analyzeVariable ctx x =
    concatEithers $
      [analyzeType ctx (variableType x)] ++
      [analyzeOuterExpression ctx (variableValue x)]

  analyzeImport :: Monoid b => a -> ImportDeclaration -> Either String b
  analyzeImport ctx x =
    Right mempty

  analyzeMethod :: Monoid b => a -> Method -> Either String b
  analyzeMethod ctx x =
    concatEithers $
      fmap (analyzeVariable ctx) (methodParameters x) ++
      [analyzeStatement ctx (methodStatement x)]

  analyzeStatement :: Monoid b => a -> Statement -> Either String b
  analyzeStatement ctx x@BlockStatement{} =
    concatEithers $
      [ analyzeStatement ctx (statementBlock x)
      , analyzeStatement ctx (nextStatement x) ]
  analyzeStatement ctx x@ExpressionStatement{} =
    concatEithers $
      [ analyzeOuterExpression ctx (statementExpression x)
      , analyzeStatement ctx (nextStatement x) ]
  analyzeStatement ctx x@LoopStatement{} =
    concatEithers $
      [ analyzeOuterExpression ctx (loopPredicate x)
      , analyzeStatement ctx (loopStatement x)
      , analyzeStatement ctx (nextStatement x) ]
  analyzeStatement ctx x@IfStatement{} =
    concatEithers $
      [ analyzeOuterExpression ctx (ifPredicate x)
      , analyzeStatement ctx (ifThenStatement x)
      , analyzeStatement ctx (ifElseStatement x)
      , analyzeStatement ctx (nextStatement x) ]
  analyzeStatement ctx x@ReturnStatement{} =
    concatEithers $
      fmap (analyzeOuterExpression ctx) (maybeToList $ returnExpression x) ++
      [ analyzeStatement ctx (nextStatement x) ]
  analyzeStatement ctx x@LocalStatement{} =
    concatEithers $
      [ analyzeVariable ctx (localVariable x)
      , analyzeStatement ctx (nextStatement x) ]
  analyzeStatement ctx x@EmptyStatement{} =
    analyzeStatement ctx (nextStatement x)
  analyzeStatement ctx TerminalStatement =
    Right mempty

  analyzeType :: Monoid b => a -> Type -> Either String b
  analyzeType ctx x =
    Right mempty
