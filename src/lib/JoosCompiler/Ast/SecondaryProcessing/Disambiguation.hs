module JoosCompiler.Ast.SecondaryProcessing.Disambiguation
  (disambiguate) where

import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types

{- TODO
    - traverse tree until we see expression name
    - if expression name, try to resolve first part as local
    - otherwise,          try to resolve as field (double check the order)
    - otherwise,          try to resolve as an import
    - otherwise,          try to resolve as a package
      in this case, we keep resolving as a package until we hit a type

   Once it's resolved, we recursively resolve the rest of the name. Needs to be
   a type name in order for the resolution to be valid. We then generate a tree
   of "accesses" (staticFieldAccess, fieldAccess, localAccess)
-}

disambiguate :: AstNode -> AstNode
disambiguate = id
