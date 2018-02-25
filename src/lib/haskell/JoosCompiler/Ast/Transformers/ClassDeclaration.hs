module JoosCompiler.Ast.Transformers.ClassDeclaration where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

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
    modifiers = []
    className = "TODO"
    super = []
    interfaces = []
    scope = Scope Nothing [] []
    methods = []
    constructor = Nothing
