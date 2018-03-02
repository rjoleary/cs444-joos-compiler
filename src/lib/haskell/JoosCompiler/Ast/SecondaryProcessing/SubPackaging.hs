module JoosCompiler.Ast.SecondaryProcessing.SubPackaging
  ( subPackage
  ) where

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
    newProgram = AstWholeProgram $ WholeProgram newPackages
    injectSubPackages :: Package -> Package
    injectSubPackages p@(Package packageName _ packageUnits) =
      Package packageName subPackages packageUnits
      where
        subPackages = filter (`isSubPackageOf` p) packages

isSubPackageOf :: Package -> Package -> Bool
isSubPackageOf (Package (Just name1) _ _) (Package (Just name2) _ _) =
  name2 `isProperPrefixOf` name1
isSubPackageOf _ _ = False
