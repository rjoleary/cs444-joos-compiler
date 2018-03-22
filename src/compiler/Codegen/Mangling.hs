module Codegen.Mangling
  ( mangleField
  , mangleMethod
  , mangleType
  ) where

import Data.List
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes


-- package p.q;
-- class A {
--   int fa = 0;
--   int ma (int i, java.lang.String s) {}
-- }

-- Field$p$q$A$fa
mangleField :: Field -> String
mangleField field = mangledCanonical
  where
    canonicalized = variableCanonicalName field -- [p, q, A, fa]
    mangledCanonical = intercalate "$" ("Field" : canonicalized)

-- Method$p$q$A$ma#int#java.lang.String#
mangleMethod :: Method -> String
mangleMethod method = mangledCanonical -- methodName method
  where
    canonicalized = methodCanonicalName method
    firstPart = intercalate "$" ("Method" : canonicalized)
    v = methodParameters method
    t = map (show . variableType) v
    secondPart = intercalate "#" t
    prepare = [firstPart, secondPart]
    mangledCanonical = (intercalate "#" prepare) ++ "#"



-- Class$p$q$A
-- Can also be interface
mangleType :: TypeDeclaration -> String
mangleType t = mangledCanonical
  where
    canonicalized = typeCanonicalName t
 --   canonicalized = typeCanonicalName s
    mangledCanonical = intercalate "$" ("Class" : canonicalized)
