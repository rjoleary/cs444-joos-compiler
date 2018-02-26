module JoosCompiler.Ast.Transformers.Types where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

data AstWrapper
  = AstClassDeclaration ClassDeclaration
  | AstCompilationUnit CompilationUnit
  | AstExpression { astExpression :: Expression }
  | AstField { astField :: Field }
  | AstType { astType :: Type }
  | AstModifier { astModifier :: Modifier }
  | AstModifiers { astModifiers :: [Modifier] }
  | AstScope Scope
  | AstTaggedToken TaggedToken
  deriving (Show)

type AstNode = Tree AstWrapper

type Transformer = [AstNode] -> TaggedParseTree -> AstWrapper

isExpression :: AstWrapper -> Bool
isExpression (AstExpression _) = True
isExpression _                 = False

getExpression :: [AstNode] -> AstWrapper
getExpression ts = rootLabel $ head $ findChildren1 isExpression ts

isField :: AstWrapper -> Bool
isField (AstField _) = True
isField _            = False

getFields :: [AstNode] -> [AstWrapper]
getFields ts = map rootLabel $ findChildren1 isExpression ts

isModifier :: AstWrapper -> Bool
isModifier (AstModifier _) = True
isModifier _               = False

isModifiers :: AstWrapper -> Bool
isModifiers (AstModifiers _) = True
isModifiers _                = False

getModifiers :: [AstNode] -> AstWrapper
getModifiers ts = rootLabel $ head $ findChildren1 isModifiers ts

isType :: AstWrapper -> Bool
isType (AstType _) = True
isType _           = False

getType :: [AstNode] -> Type
getType ts = astType $ rootLabel typeNode
  where
    typeNode = head $ findChildren1 isType ts
