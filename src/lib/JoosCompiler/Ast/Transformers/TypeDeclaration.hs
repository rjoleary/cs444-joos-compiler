module JoosCompiler.Ast.Transformers.TypeDeclaration
  ( typeDeclarationTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

typeDeclarationTransformer :: Transformer
typeDeclarationTransformer transformedChildren t@(Node label _) =
  AstTypeDeclaration $
  TypeDeclaration
  { typeName = _typeName
  , classModifiers = modifiers
  , isInterface = _isInterface
  , super = _super
  , interfaces = _interfaces
  , classFields = vars
  , methods = _methods
  , constructors = _constructors
  , typeCanonicalName = error "typeCanonicalName used before initialization"
  }
  where
    _isInterface = (kInterfaceDeclaration == tokenName label)
    modifiers = astModifiers $ getClassModifiers transformedChildren
    _typeName = getTypeNameFromDeclaration t
    _superTemp = getSuperName t
    _super
      | _superTemp == [] = ["java", "lang", "Object"]
      | otherwise = _superTemp
    _interfaces = getInterfaceNames t
    vars = map astField $ getTypeFields transformedChildren
    _methods = getTypeMethods transformedChildren
    _constructors = getClassConstructors transformedChildren

getTypeFields :: [AstNode] -> [AstWrapper]
getTypeFields ts = map rootLabel fieldNodes
  where
    fieldNodes = mconcat $ map (findDirectChildren isField isMethod) ts

getTypeMethods :: [AstNode] -> [Method]
getTypeMethods ts = map (astMethod . rootLabel) methodNodes
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
