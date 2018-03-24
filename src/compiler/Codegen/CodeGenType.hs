{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Codegen.CodeGenType
  ( codeGenType
  ) where

import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Visitor.Analysis
import Codegen.X86
import qualified Codegen.X86 as X86

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

-- This gets placed between each recursive call to generateExpression to indent
-- the block and add extra debug information.
generateExpression' :: Context -> Expression -> Asm Type
generateExpression' ctx e = do
  comment (show e)
  indent (generateExpression ctx e)

generateExpression :: Context -> Expression -> Asm Type

-- Add is a special binary operator because it is overloaded for strings.
generateExpression ctx e@(BinaryOperation Add x y) = do
  comment (show e)
  t1 <- generateExpression' ctx x
  push Eax
  t2 <- generateExpression' ctx y
  mov Ebx Eax
  pop Eax
  add Eax Ebx
  return (Type Int False) -- TODO: strings

generateExpression ctx e@(BinaryOperation op x y) = do
  comment (show e)
  t1 <- generateExpression' ctx x
  push Eax
  t2 <- generateExpression' ctx y
  mov Ebx Eax
  pop Eax
  binaryOperatorAsm op


binaryOperatorAsm :: BinaryOperator -> Asm Type
binaryOperatorAsm Multiply     = imul Eax Ebx >> return (Type Int False)
binaryOperatorAsm Modulus      = idiv Ebx >> mov Edx Eax >> return (Type Int False)
binaryOperatorAsm Divide       = idiv Ebx >> return (Type Int False)
binaryOperatorAsm Subtract     = sub Eax Ebx >> return (Type Int False)
binaryOperatorAsm LazyAnd      = X86.and Eax Ebx >> return (Type Boolean False)
binaryOperatorAsm LazyOr       = X86.or Eax Ebx >> return (Type Boolean False)
binaryOperatorAsm Less         = cmp Eax Ebx >> setl Al >> return (Type Boolean False)
binaryOperatorAsm Greater      = cmp Eax Ebx >> setg Al >> return (Type Boolean False)
binaryOperatorAsm LessEqual    = cmp Eax Ebx >> setle Al >> return (Type Boolean False)
binaryOperatorAsm GreaterEqual = cmp Eax Ebx >> setge Al >> return (Type Boolean False)
binaryOperatorAsm Equality     = cmp Eax Ebx >> sete Al >> return (Type Boolean False)
binaryOperatorAsm Inequality   = cmp Eax Ebx >> setne Al >> return (Type Boolean False)
