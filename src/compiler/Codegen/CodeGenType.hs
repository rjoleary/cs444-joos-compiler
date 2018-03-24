{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Codegen.CodeGenType
  ( codeGenType
  ) where

import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Visitor.Analysis
import Codegen.X86

codeGenType :: TypeDeclaration -> Either String (Asm ())
codeGenType = analyze' CodeGenType

data CodeGenType = CodeGenType

instance Analysis CodeGenType (Asm ()) where
  analyze ctx (AstTypeDeclaration t@TypeDeclaration{isInterface=False}) = Right $ do
    label t
    dd (I 0x12345678)
    comment (show (length (classFields t)) ++ " fields")
    mapM_ (\field -> do
      comment (variableName field)
      dd (I 0)
      ) (classFields t) -- TODO: only static fields

  analyze ctx (AstTypeDeclaration t@TypeDeclaration{isInterface=True}) = Right $ do
    label t
    dd (I 0x87654321)

  -- Everything else propagates.
  analyze ctx x = propagateAnalyze ctx x

data Context

generateExpression :: Context -> Expression -> Asm Type
generateExpression ctx (BinaryOperation Multiply x y) = do
  t1 <- generateExpression ctx x
  push Eax
  t2 <- generateExpression ctx y
  mov Ebx Eax
  pop Eax
  imul Eax Ebx
  return (Type Int False) -- TODO: strings

generateExpression ctx (BinaryOperation Modulus x y) = do
  t1 <- generateExpression ctx x
  push Eax
  t2 <- generateExpression ctx y
  mov Ebx Eax
  pop Eax
  idiv Ebx
  mov Edx Eax
  return (Type Int False)

generateExpression ctx (BinaryOperation Divide x y) = do
  t1 <- generateExpression ctx x
  push Eax
  t2 <- generateExpression ctx y
  mov Ebx Eax
  pop Eax
  idiv Ebx
  return (Type Int False)

generateExpression ctx (BinaryOperation Add x y) = do
  t1 <- generateExpression ctx x
  push Eax
  t2 <- generateExpression ctx y
  mov Ebx Eax
  pop Eax
  add Eax Ebx
  return (Type Int False)

generateExpression ctx (BinaryOperation Subtract x y) = do
  t1 <- generateExpression ctx x
  push Eax
  t2 <- generateExpression ctx y
  mov Ebx Eax
  pop Eax
  sub Eax Ebx
  return (Type Int False)
