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
  , constructor = _constructor
  }
  where
    _isInterface = (kInterfaceDeclaration == tokenName label)
    modifiers = [] -- astModifiers $ getClassModifiers transformedChildren
    _className = getClassNameFromDeclaration t
    _super = getSuperName t
    _interfaces = getInterfaceNames t
    vars = map astField $ getClassFields transformedChildren
    _methods = []
    _constructor = Nothing

getClassFields :: [AstNode] -> [AstWrapper]
getClassFields ts = map rootLabel fieldNodes
  where
    fieldNodes = mconcat $ map (findDirectChildren isField isMethod) ts

getSuperName :: TaggedParseTree -> Name
getSuperName t = extractName nameParts
  where
    nameNodes = findChildrenByTokenName kName t
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
      mconcat $
      map (findDirectChildrenByTokenName1 kName kInterfaceTypeList) $
      map subForest interfaceNodes
    nameNodesList = map (findChildrenByTokenName kIdentifier) parentNameNodes

getClassModifiers :: [AstNode] -> AstWrapper
getClassModifiers ts = rootLabel $ findDirectChild1 isModifiers isMethod ts
