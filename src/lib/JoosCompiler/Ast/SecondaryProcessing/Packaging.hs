module JoosCompiler.Ast.SecondaryProcessing.Packaging
  (
    packageProgram
  ) where

import           Data.Maybe
import           Data.List
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

unique :: Eq a => [a] -> [a]
unique = foldr (\x seen -> if (x `elem` seen) then seen else x:seen) []

packageProgram  :: [AstNode] -> AstNode
packageProgram unitNodes = Node program unitNodes
  where
    groupedByPackages = groupByPackage unitNodes
    units = map (astCompilationUnit . rootLabel) unitNodes
    program = AstWholeProgram $ WholeProgram groupedByPackages units

-- Assumes that root nodes are CompilationUnits
groupByPackage :: [AstNode] -> [Package]
groupByPackage unitNodes = packages
  where
    compilationUnits = map (astCompilationUnit . rootLabel) unitNodes
    -- TODO unique
    packageNames = unique $ map cuPackage compilationUnits
    packages = map groupUnitsByPackageName packageNames
    groupUnitsByPackageName :: Name -> Package
    groupUnitsByPackageName _packageName =
      Package _packageName packageUnitNames
      where
        packageUnits = filter ((== _packageName) . cuPackage) compilationUnits
        packageUnitNames = map cuTypeName packageUnits
