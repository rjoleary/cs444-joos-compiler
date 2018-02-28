module JoosCompiler.Ast.Transformers.MethodTransformers
  ( methodTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

methodTransformer :: Transformer
methodTransformer transformedChildren t@(Node label _) =
  AstMethod $
  Method
  { methodType = _type
  , methodModifiers = _modifiers
  , methodName = _name
  , statements = _statements
  }
  where
    _type = getMethodType transformedChildren
    _modifiers = astModifiers $ getMethodModifiers transformedChildren
    _name = getMethodName t
    _statements = getStatements transformedChildren

getMethodModifiers :: [AstNode] -> AstWrapper
getMethodModifiers ts =
  rootLabel $ head $ findDirectChildren1 isModifiers isMethod ts

getMethodName :: TaggedParseTree -> Name
getMethodName (Node _ ts) = extractName [nameNode]
  where
    methodHeader = head ts
    declaratorNode = subForest methodHeader !! 2
    nameNode =
      findDirectChildByTokenName kIdentifier kFormalParameterList declaratorNode

getMethodType :: [AstNode] -> Type
getMethodType ts = extractType typeLabel
  where
    methodHeader = head ts
    typeNode = subForest methodHeader !! 1
    typeLabel = rootLabel typeNode
    extractType :: AstWrapper -> Type
    extractType (AstType t) = t
    extractType _           = error "Unexpected AST type in methodtype"

getStatements :: [AstNode] -> [Statement]
getStatements _ = []
