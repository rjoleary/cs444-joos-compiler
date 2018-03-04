module JoosCompiler.Ast.SecondaryProcessing.SubPackaging
  (
    -- subPackageFromProgram
  ) where

import           Data.List
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils

-- subPackage :: [Package] -> SubPackageMap
-- subPackage t@(Node oldProgram children) = ()
--   where
--     packages = programPackages $ astWholeProgram oldProgram
--     newPackages = map injectSubPackages packages
--     newProgram = AstWholeProgram $ WholeProgram newPackages
--     injectSubPackages :: Package -> Package
--     injectSubPackages p@(Package _packageName _ packageUnits) =
--       Package _packageName subPackagesMap packageUnits
--       where
--         subPackages = filter (`isSubPackageOf` p) packages
--         subPackageNames = map (fromJust . packageName) subPackages
--         subPackagesMap = zip subPackageNames subPackages -- AList = association list

-- subPackageProgram  :: AstNode -> AstNode
-- subPackageProgram program = subPackageFromPackages packages


subPackagefromPackages :: [Package] -> SubPackage
subPackagefromPackages packages = SubPackage defaultPackage subPackageMap
  where
    defaultPackageMatches = filter ((==) [] . packageName) packages
    defaultPackage = if length defaultPackageMatches > 0
      then Just $ head defaultPackageMatches
      else Nothing
    _subPackages = filter ((/=) [] . packageName) packages
    subPackageAList = map (\p -> (packageName p, p)) _subPackages
    subPackageMap = subPackage subPackageAList

subPackage :: [(Name, Package)] -> SubPackageMap
subPackage packageAList = subPackageMapEntries
  where
    grouped = groupBy startsWithSameName packageAList
    subPackageMapEntries = map makeSubPackageMapEntry grouped

startsWithSameName :: (Name, Package) -> (Name, Package) -> Bool
startsWithSameName p1 p2 = (head $ fst p1) == (head $ fst p2)


-- This function expects that
-- 1. No input tuple has fst = []
-- 2. All input tuples have the same String at the beginning of their Name
makeSubPackageMapEntry :: [(Name, Package)] -> SubPackageMapEntry
makeSubPackageMapEntry packageAList = (packageNamePart, thisSubPackage)
  where
    trimmed = map trimPackageName packageAList
    packageNamePart = head $ fst $ head packageAList
    thisPackage = lookup [] trimmed
    subPackages = filter (\p -> fst p /= []) trimmed
    subPackageMap =  subPackage subPackages
    thisSubPackage = SubPackage thisPackage subPackageMap

trimPackageName :: (Name, Package) -> (Name, Package)
trimPackageName (oldName, p) = (newName, p)
  where
    newName = tail oldName

isSubPackageOf :: Package -> Package -> Bool
isSubPackageOf (Package name1 _ _) (Package name2 _ _) =
  name2 `isProperPrefixOf` name1
