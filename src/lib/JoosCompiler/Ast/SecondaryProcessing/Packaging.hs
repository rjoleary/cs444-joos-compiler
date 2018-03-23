module JoosCompiler.Ast.SecondaryProcessing.Packaging
  (
    packageProgram
  ) where

import           Data.Maybe
import           Data.List
import           Data.List.Unique
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

packageProgram  :: [AstNode] -> AstNode
packageProgram unitNodes = Node program unitNodes
  where
    groupedByPackages = groupByPackage unitNodes
    units = map (astCompilationUnit . rootLabel) unitNodes
    program = AstWholeProgram $ WholeProgram groupedByPackages units

startsWithSameName :: (Name, Package) -> (Name, Package) -> Bool
startsWithSameName p1 p2 = (head $ fst p1) == (head $ fst p2)

-- Assumes that root nodes are CompilationUnits
groupByPackage :: [AstNode] -> [Package]
groupByPackage unitNodes = packages
  where
    compilationUnits = map (astCompilationUnit . rootLabel) unitNodes
    -- TODO unique
    packageNames = map cuPackage compilationUnits
    packages = map groupUnitsByPackageName packageNames
    groupUnitsByPackageName :: Name -> Package
    groupUnitsByPackageName _packageName =
      Package _packageName packageUnitNames
      where
        packageUnits = filter ((== _packageName) . cuPackage) compilationUnits
        packageUnitNames = map cuTypeName packageUnits
