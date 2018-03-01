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
  , formalParameters = _formalParams
  , statements = _statements
  }
  where
    _type = getMethodType transformedChildren
    _modifiers = astModifiers $ getMethodModifiers transformedChildren
    _name = getMethodName t
    _formalParams = getFormalParams transformedChildren
    _statements = getStatements transformedChildren

getFormalParams :: [AstNode] -> [Field]
getFormalParams ts = map convertToField formalParamNodes
  where
    formalParamNodes = findAstChildrenByTokenName1 kFormalParameter ts
    convertToField :: AstNode -> Field
    convertToField paramNode =
      Field
      { fieldType = _type
      , fieldModifiers = []
      , fieldName = [_name]
      , fieldValue = (Literal "3")
      , isClassField = True
      }
      where
        typeNode = (subForest paramNode) !! 0
        nameNode = (subForest paramNode) !! 1
        _type = astType $ rootLabel typeNode
        _name = tokenString $ astTaggedToken $ rootLabel nameNode

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
