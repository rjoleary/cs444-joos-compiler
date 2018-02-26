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
    -- There should only be one
    modifiersWrappers = mconcat $ map getClassModifiers transformedChildren
    modifiers = astModifiers $ head modifiersWrappers
    className = getClassNameFromDeclaration t
    super = []
    interfaces = []
    scope = Scope Nothing [] []
    methods = []
    constructor = Nothing

getClassModifiers :: AstNode -> [AstWrapper]
getClassModifiers t = map rootLabel $ findChildren isModifiers t
