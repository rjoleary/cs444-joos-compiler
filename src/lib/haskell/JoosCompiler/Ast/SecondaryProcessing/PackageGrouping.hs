module JoosCompiler.Ast.SecondaryProcessing.PackageGrouping
  ( groupByPackage
  ) where

import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants

-- Assumes that root nodes are CompilationUnits
groupByPackage :: [AstNode] -> AstNode
groupByPackage unitNodes = Node program unitNodes
  where
    compilationUnits = map (astCompilationUnit . rootLabel) unitNodes
    packageNames = map cuPackage compilationUnits
    packages = map groupUnitsByPackageName packageNames
    program = AstWholeProgram $ WholeProgram packages
    groupUnitsByPackageName :: Maybe Name -> Package
    groupUnitsByPackageName maybePackageName =
      Package _packageName [] packageUnitsAList
      where
        _packageName = fromMaybe [] maybePackageName
        packageUnits = filter ((== maybePackageName) . cuPackage) compilationUnits
        packageUnitsAList = zip (map cuTypeName packageUnits) packageUnits
