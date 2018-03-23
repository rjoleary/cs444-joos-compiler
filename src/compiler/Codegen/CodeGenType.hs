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
    label ("Class$" ++ typeName t) -- TODO: use the mangle
    dd (I 0x12345678)

  analyze ctx (AstTypeDeclaration t@TypeDeclaration{isInterface=True}) = Right $ do
    label ("Class$" ++ typeName t) -- TODO: use the mangle
    dd (I 0x87654321)

  -- Everything else propagates.
  analyze ctx x = propagateAnalyze ctx x
