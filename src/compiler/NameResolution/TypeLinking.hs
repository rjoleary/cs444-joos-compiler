module NameResolution.TypeLinking
  ( typeLinkingRules
  ) where

import           AstRule
import           Data.List.Unique
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.TreeUtils

noTwoFieldsSameName :: AstRulePredicate
noTwoFieldsSameName t = not $ all allUnique classFieldNames
  where
    classNodes = findChildren isClassDeclaration t
    _classFields = map (classFields . astClass . rootLabel) classNodes
    classFieldNames = map (map fieldName) _classFields

typeLinkingRules :: [AstRule]
typeLinkingRules =
  [("Class contains duplicate field names", noTwoFieldsSameName)]
