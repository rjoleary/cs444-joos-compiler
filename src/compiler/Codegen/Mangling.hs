module Codegen.Mangling
  ( mangleField
  , mangleMethod
  , mangleType
  ) where

import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes

-- package p.q;
-- class A {
--   int fa = 0;
--   int ma (int i, java.lang.String s) {}
-- }

-- Field$p$q$A$fa
mangleField :: Field -> String
mangleField field = variableName field
  where
    canonicalized = variableCanonicalName field

-- Method$p$q$A$ma#int#java.lang.String#
mangleMethod :: Method -> String
mangleMethod method = methodName method
  where
    canonicalized = methodCanonicalName method

-- Class$p$q$A
-- Can also be interface
mangleType :: TypeDeclaration -> String
mangleType t = typeName t
  where
    canonicalized = typeCanonicalName t
