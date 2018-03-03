module NameResolution.TypeLinking
  ( typeLinkingRules
  ) where

import           AstRule
import           Data.List.Unique
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TreeUtils

getCompilationUnits t = compilationUnits
  where
    unitNodes = findChildren isCompilationUnit t
    compilationUnits = map (astCompilationUnit . rootLabel) unitNodes

noSingleTypeImportClashesWithClass :: AstRulePredicate
noSingleTypeImportClashesWithClass t = not $ all noClashPresent compilationUnits
  where
    compilationUnits = getCompilationUnits t
    noClashPresent unit = all (/= cName) importNames
      where
        _imports = imports unit
        cName = cuClassName unit
        simpleImports = filter (not . onDemand) _imports
        importNames = map (last . importName) simpleImports

-- noSingleTypeImportClashWithEachOther

-- allTypesResolveToClassDeclaredInSomeFile

-- When a fully qualified name resolves to a type, no strict prefix of
-- the fully qualified name can resolve to a type in the same
-- environment
-- typeFullyQualifiedNameNotPrefixedByTypeName


typeLinkingRules :: [AstRule]
typeLinkingRules =
  [ ("No single-type import clashes with class", noSingleTypeImportClashesWithClass)
  ]
