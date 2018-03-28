{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Codegen.Mangling
  ( Mangleable(..)
  , Init(..)
  ) where

import Data.List
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes


-- package p.q;
-- class A {
--   int fa = 0;
--   int ma (int i, java.lang.String s) {}
-- }

class Mangleable a where
    mangle :: a -> String

-- Field$p$q$A$fa
instance Mangleable Variable where
  mangle field = modifier ++ "Field$" ++ intercalate "$" (variableCanonicalName field)
    where modifier = if isFieldStatic field then "Static" else ""

-- Method$p$q$A$ma#int#java.lang.String#
instance Mangleable Method where
  mangle method = mangledCanonical -- methodName method
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
instance Mangleable TypeDeclaration where
  mangle t = mangledCanonical
    where
      canonicalized = typeCanonicalName t
      mangledCanonical = intercalate "$" ("Class" : canonicalized)

data Init = Init TypeDeclaration

-- Init$Class$p$q$A
instance Mangleable Init where
  mangle (Init t) = "Init$" ++ mangle t

instance Mangleable String where
  mangle s = s
