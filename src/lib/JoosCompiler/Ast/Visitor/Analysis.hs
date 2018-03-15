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
      fmap (analyzeField ctx) (classFields x) ++
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
  analyzeExpression ctx (NewArrayExpression name arg) =
    analyzeOuterExpression ctx arg
  analyzeExpression ctx (CastExpression t e) =
    concatEithers $ [analyzeType ctx t, analyzeOuterExpression ctx e]
  analyzeExpression ctx (InstanceOfExpression e t) =
    concatEithers $ [analyzeOuterExpression ctx e, analyzeType ctx t]
  analyzeExpression ctx (ArrayExpression e1 e2) =
    concatEithers $ [analyzeOuterExpression ctx e1, analyzeOuterExpression ctx e2]

  analyzeField :: Monoid b => a -> Field -> Either String b
  analyzeField ctx x =
    concatEithers $
      [analyzeType ctx (fieldType x)] ++
      [analyzeOuterExpression ctx (fieldValue x)]

  analyzeImport :: Monoid b => a -> ImportDeclaration -> Either String b
  analyzeImport ctx x =
    Right mempty

  analyzeLocalVariable :: Monoid b => a -> Local -> Either String b
  analyzeLocalVariable ctx x =
    concatEithers $
      [analyzeType ctx (localType x)] ++
      [analyzeOuterExpression ctx (localValue x)]

  analyzeMethod :: Monoid b => a -> Method -> Either String b
  analyzeMethod ctx x =
    concatEithers $
      fmap (analyzeLocalVariable ctx) (methodParameters x) ++
      fmap (analyzeOuterStatement ctx) (methodStatements x)

  -- TODO: remove this node
  analyzeOuterStatement :: Monoid b => a -> Statement -> Either String b
  analyzeOuterStatement ctx (Statement x) = analyzeStatement ctx x

  analyzeStatement :: Monoid b => a -> InnerStatement -> Either String b
  analyzeStatement ctx x@BlockStatement{} =
    concatEithers $ fmap (analyzeOuterStatement ctx) (blockStatements x)
  analyzeStatement ctx x@ExpressionStatement{} =
    analyzeOuterExpression ctx (statementExpression x)
  analyzeStatement ctx x@LoopStatement{} =
    concatEithers $
      analyzeOuterExpression ctx (loopPredicate x) :
      fmap (analyzeOuterStatement ctx) (loopStatements x)
  analyzeStatement ctx x@IfStatement{} =
    concatEithers $
      [ analyzeOuterExpression ctx (ifPredicate x)
      , analyzeOuterStatement ctx (ifThenStatement x)
      , analyzeOuterStatement ctx (ifElseStatement x) ]
  analyzeStatement ctx x@ReturnStatement{} =
    concatEithers $ fmap (analyzeOuterExpression ctx) (maybeToList $ returnExpression x)
  analyzeStatement ctx (LocalStatement x) =
    analyzeLocalVariable ctx x
  analyzeStatement ctx EmptyStatement =
    Right mempty

  analyzeType :: Monoid b => a -> Type -> Either String b
  analyzeType ctx x =
    Right mempty
