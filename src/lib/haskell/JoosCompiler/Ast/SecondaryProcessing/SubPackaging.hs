module JoosCompiler.Ast.SecondaryProcessing.SubPackaging
  ( subPackage
  ) where

import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

subPackage :: AstNode -> AstNode
subPackage t@(Node oldProgram children) = Node newProgram children
  where
    packages = programPackages $ astWholeProgram oldProgram
    newPackages = map injectSubPackages packages
    newPackageNames = map (fromJust . packageName) newPackages
    newPackagesAList = zip newPackageNames newPackages
    unpackagedUnits = getUnpackagedUnits children
    rootPackage = Package Nothing newPackagesAList unpackagedUnits
    newProgram = AstWholeProgram $ WholeProgram [rootPackage]
    injectSubPackages :: Package -> Package
    injectSubPackages p@(Package _packageName _ packageUnits) =
      Package _packageName subPackagesAList packageUnits
      where
        subPackages = filter (`isSubPackageOf` p) packages
        subPackageNames = map (fromJust . packageName) subPackages
        subPackagesAList = zip subPackageNames subPackages -- AList = association list

getUnpackagedUnits :: [AstNode] -> PackageCompilationUnits
getUnpackagedUnits ts = zip classNames filtered
  where
    units = map (astCompilationUnit . rootLabel) ts
    _filtered = filter ((== Nothing) . cuPackage) units
    filtered = filter ((/= Nothing) . classDecl) _filtered
    classNames = map (className . fromJust . classDecl) filtered

isSubPackageOf :: Package -> Package -> Bool
isSubPackageOf (Package (Just name1) _ _) (Package (Just name2) _ _) =
  name2 `isProperPrefixOf` name1
isSubPackageOf _ _ = False
