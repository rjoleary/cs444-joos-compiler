module JoosCompiler.Ast.Visitor.Analysis
  (
  Analysis
  ) where

import Data.Maybe
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Error

concatEithers :: Monoid a => [Either String a] -> Either String a
concatEithers = fmap mconcat . foldEither

-- Analysis takes the entire tree and outputs a single value.
class Monoid b => Analysis b where
  analyzeTypeDeclaration :: a -> TypeDeclaration -> Either String b
  analyzeTypeDeclaration ctx x =
    concatEithers $
      fmap (analyzeField ctx) (classFields x) ++
      fmap (analyzeMethod ctx) (methods x) ++
      fmap (analyzeMethod ctx) (constructors x)

  analyzeWholeProgram :: a -> WholeProgram -> Either String b
  analyzeWholeProgram ctx x =
    concatEithers $
      fmap (analyzeCompilationUnit ctx) (programCus x)

  analyzeCompilationUnit :: a -> CompilationUnit -> Either String b
  analyzeCompilationUnit ctx x =
    concatEithers $
      fmap (analyzeImport ctx) (imports x) ++
      fmap (analyzeTypeDeclaration ctx) (maybeToList (typeDecl x))

  -- TODO: remove this node
  analyzeOuterExpression :: a -> Expression -> Either String b
  analyzeOuterExpression ctx (Expression _ x) = analyzeExpression ctx x

  analyzeExpression :: a -> InnerExpression -> Either String b
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

  analyzeField :: a -> Field -> Either String b
  analyzeField ctx x =
    concatEithers $
      [analyzeType ctx (fieldType x)] ++
      [analyzeOuterExpression ctx (fieldValue x)]

  analyzeImport :: a -> ImportDeclaration -> Either String b
  analyzeImport ctx x =
    Right mempty

  analyzeLocalVariable :: a -> Local -> Either String b
  analyzeLocalVariable ctx x =
    concatEithers $
      [analyzeType ctx (localType x)] ++
      [analyzeOuterExpression ctx (localValue x)]

  analyzeMethod :: a -> Method -> Either String b
  analyzeMethod ctx x = Right mempty -- TODO

  analyzeStatement :: a -> Statement -> Either String b
  analyzeStatement ctx x = Right mempty -- TODO

  analyzeType :: a -> Type -> Either String b
  analyzeType ctx x =
    Right mempty
