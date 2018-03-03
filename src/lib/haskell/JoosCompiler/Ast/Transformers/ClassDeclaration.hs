module JoosCompiler.Ast.Transformers.ClassDeclaration
  ( classDeclarationTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

classDeclarationTransformer :: Transformer
classDeclarationTransformer transformedChildren t@(Node label _) =
  AstClassDeclaration $
  ClassDeclaration
  { className = _className
  , classModifiers = modifiers
  , isInterface = _isInterface
  , super = _super
  , interfaces = _interfaces
  , classFields = vars
  , methods = _methods
  , constructors = _constructors
  }
  where
    _isInterface = (kInterfaceDeclaration == tokenName label)
    modifiers = astModifiers $ getClassModifiers transformedChildren
    _className = getClassNameFromDeclaration t
    _superTemp = getSuperName t
    _super
      | _superTemp == [] = [kObject]
      | otherwise = _superTemp
    _interfaces = getInterfaceNames t
    vars = map astField $ getClassFields transformedChildren
    _methods = getClassMethods transformedChildren
    _constructors = getClassConstructors transformedChildren

getClassFields :: [AstNode] -> [AstWrapper]
getClassFields ts = map rootLabel fieldNodes
  where
    fieldNodes = mconcat $ map (findDirectChildren isField isMethod) ts

getClassMethods :: [AstNode] -> [Method]
getClassMethods ts = map (astMethod . rootLabel) methodNodes
  where
    methodNodes = findChildren1 isMethod ts

getClassConstructors :: [AstNode] -> [Method]
getClassConstructors ts = map (astConstructor . rootLabel) constructorNodes
  where
    constructorNodes = findChildren1 isConstructor ts

getSuperName :: TaggedParseTree -> Name
getSuperName t = extractName nameParts
  where
    superNodes = findChildrenByTokenName kSuper t
    nameNodes = findChildrenByTokenName1 kName superNodes
    parentNameNode = head $ nameNodes
    nameParts =
      if length nameNodes > 0
        then findChildrenByTokenName kIdentifier parentNameNode
        else []

getInterfaceNames :: TaggedParseTree -> [Name]
getInterfaceNames t = map extractName nameNodesList
  where
    interfaceNodes = findChildrenByTokenName kInterfaceTypeList t
    parentNameNodes =
      map head $
      map (findDirectChildrenByTokenName1 kName kInterfaceTypeList) $
      map subForest interfaceNodes
    nameNodesList = map (findChildrenByTokenName kIdentifier) parentNameNodes

getClassModifiers :: [AstNode] -> AstWrapper
getClassModifiers ts = rootLabel $ head $ findChildren1 isModifiers ts
