module JoosCompiler.Ast.Transformers.MethodTransformers
  ( constructorTransformer
  , methodTransformer
  ) where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
import           JoosCompiler.Ast.Utils
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

abstractMethodTransformer :: Transformer
abstractMethodTransformer transformedChildren t@(Node label _) =
  AstMethod $
  Method
  { methodReturn = _type
  , methodModifiers = _modifiers
  , methodName = _name
  , methodParameters = _formalParams
  , methodStatement = TerminalStatement
  , methodCanonicalName = [_name] -- TODO(Ahmed) make error (the two below as well)
  }
  where
    _type = getAbstractMethodType t
    _modifiers = astModifiers $ getMethodModifiers transformedChildren
    _name = getAbstractMethodName t
    _formalParams = getFormalParams transformedChildren

constructorTransformer :: Transformer
constructorTransformer transformedChildren t@(Node label [modifiers, declaration, body]) =
  AstConstructor $
  Method
  { methodReturn = Void
  , methodModifiers = _modifiers
  , methodName = []
  , methodParameters = _formalParams
  , methodStatement = _statements
  , methodCanonicalName = []
  }
  where
    _modifiers = astModifiers $ getMethodModifiers transformedChildren
    _formalParams = getFormalParams transformedChildren
    _statements = constructorBodyTransformer body

methodTransformer :: Transformer
methodTransformer transformedChildren t@(Node label [header, body]) =
  AstMethod $
  Method
  { methodReturn = _type
  , methodModifiers = _modifiers
  , methodName = _name
  , methodParameters = _formalParams
  , methodStatement = _statements
  , methodCanonicalName = [_name]
  }
  where
    _type = getMethodType header
    _modifiers = astModifiers $ getMethodModifiers transformedChildren
    _name = getMethodName t
    _formalParams = getFormalParams transformedChildren
    _statements = methodBodyTransformer body

getFormalParams :: [AstNode] -> [Variable]
getFormalParams ts = map convertToLocal formalParamNodes
  where
    formalParamNodes = findAstChildrenByTokenName1 kFormalParameter ts
    convertToLocal :: AstNode -> Variable
    convertToLocal paramNode =
      Variable
      { variableType = _type
      , variableModifiers = []
      , variableName = _name
      , variableValue = LiteralExpression $ StringLiteral "TODO"
      , variableCanonicalName = error "variableCanonicalName invalid for locals"
      }
      where
        typeNode = (subForest paramNode) !! 0
        nameNode = (subForest paramNode) !! 1
        _type = typeTransformer $ fmap astTaggedToken $ typeNode
        _name = tokenString $ astTaggedToken $ rootLabel nameNode

getMethodModifiers :: [AstNode] -> AstWrapper
getMethodModifiers ts =
  rootLabel $ head $ findDirectChildren1 isModifiers isMethod ts

getMethodName :: TaggedParseTree -> String
getMethodName (Node _ ts) = head $ extractName [nameNode]
  where
    methodHeader = head ts
    declaratorNode = subForest methodHeader !! 2
    nameNode =
      findDirectChildByTokenName kIdentifier kFormalParameterList declaratorNode

getAbstractMethodName :: TaggedParseTree -> String
getAbstractMethodName methodHeader@(Node _ ts) = head $ extractName [nameNode]
  where
    declaratorNode = subForest methodHeader !! 2
    nameNode =
      findDirectChildByTokenName kIdentifier kFormalParameterList declaratorNode

getMethodType :: TaggedParseTree -> Type
getMethodType header@(Node _ [_, (Node (TaggedToken "void" _ _ _) _), _]) = Void
getMethodType header@(Node _ [_, t, _]) = typeTransformer t

getAbstractMethodType :: TaggedParseTree -> Type
getAbstractMethodType header@(Node _ [_, (Node (TaggedToken "void" _ _ _) _), _, _]) = Void
getAbstractMethodType header@(Node _ [_, t, _, _]) = typeTransformer t

getStatements :: [AstNode] -> [Statement]
getStatements _ = []
