module NameResolution.EnvironmentBuilding
  ( environmentBuildingRules
  ) where

import           AstRule
import           Data.List.Unique
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TreeUtils

noTwoFieldsSameName :: AstRulePredicate
noTwoFieldsSameName t = not $ all allUnique classFieldNames
  where
    classNodes = findChildren isTypeDeclaration t
    _classFields = map (classFields . astClass . rootLabel) classNodes
    classFieldNames = map (map fieldName) _classFields

noTwoLocalsSameName :: AstRulePredicate
noTwoLocalsSameName t = not $ all allUnique localNames
  where
    blockNodes = findChildren isBlock t
    blocks = map (astBlock . rootLabel) blockNodes
    scopes = map blockScope blocks
    locals = map flattenScope scopes
    localNames = map (map localName) locals

noTwoTypesSameCanonicalName :: AstRulePredicate
noTwoTypesSameCanonicalName t = not $ allUnique qualified
  where
    unitNodes = subForest t
    units = map (astCompilationUnit . rootLabel) unitNodes
    typeUnits = filter ((/= Nothing) . typeDecl) units
    qualified = map qualifyTypeName typeUnits

environmentBuildingRules :: [AstRule]
environmentBuildingRules =
  [ ("Class contains duplicate field names", noTwoFieldsSameName)
  , ( "Duplicate identifier for locals in overlapping scopes"
    , noTwoLocalsSameName)
  , ( "Duplicate fully-qualified class/interface name"
    , noTwoTypesSameCanonicalName)
  ]
