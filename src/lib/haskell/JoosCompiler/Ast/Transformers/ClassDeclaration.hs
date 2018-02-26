module JoosCompiler.Ast.Transformers.ClassDeclaration where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

classDeclarationTransformer :: Transformer
classDeclarationTransformer transformedChildren t@(Node label children) =
  AstClassDeclaration $
  ClassDeclaration
  { isInterface = isInterface
  , classModifiers = modifiers
  , className = className
  , super = super
  , interfaces = interfaces
  , classScope = scope
  , methods = methods
  , constructor = constructor
  }
  where
    isInterface = (kInterfaceDeclaration == tokenName label)
    modifiers = astModifiers $ getClassModifiers transformedChildren
    className = getClassNameFromDeclaration t
    super = getSuperName t
    interfaces = getInterfaceNames t
    scope = Scope Nothing [] []
    methods = []
    constructor = Nothing

getClassModifiers :: [AstNode] -> AstWrapper
getClassModifiers ts = rootLabel $ head $ findChildren1 isModifiers ts

getSuperName :: TaggedParseTree -> Name
getSuperName t = extractName nameNodes
  where
    parentNameNode = head $ findChildrenByTokenName kName t
    nameNodes = findChildrenByTokenName kIdentifier parentNameNode

getInterfaceNames :: TaggedParseTree -> [Name]
getInterfaceNames t = map extractName nameNodesList
  where
    interfaceNodes = findChildrenByTokenName kInterfaceTypeList t
    parentNameNodes =
      mconcat $
      map (findDirectChildrenByTokenName1 kName kInterfaceTypeList) $
      map subForest interfaceNodes
    nameNodesList = map (findChildrenByTokenName kIdentifier) parentNameNodes

extractName :: [TaggedParseTree] -> Name
extractName nameNodes = map (tokenString . rootLabel) nameNodes
