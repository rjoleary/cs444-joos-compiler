module JoosCompiler.Ast where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.Treeify

import           JoosCompiler.Ast.Transformers

-- We first transform children so root has access to them
cstToAstTransform :: Transformer -> TaggedParseTree -> AstNode
cstToAstTransform f t = Node transformedRoot transformedChildren
  where
    transformedChildren = map cstToAst $ subForest t
    transformedRoot = f transformedChildren t

getTransformer :: TaggedParseTree -> Transformer
getTransformer t@(Node label _)
  | (tokenName label) == kBlock = blockTransformer
  | (tokenName label) `elem` [kClassDeclaration, kInterfaceDeclaration] =
    classDeclarationTransformer
  | (tokenName label) == kFieldDeclaration = fieldTransformer
  | (tokenName label) == kLocalVariableDeclaration = localVariableTransformer
  | (tokenName label) == kMethodDeclaration = methodTransformer
  | (tokenName label) == kModifier = modifierTransformer
  | (tokenName label) == kModifiers = modifiersTransformer
  | (tokenName label) == kBlockStatement = statementTransformer
  | (tokenName label) `elem` [kType, kVoid] = typeTransformer
  | otherwise = cstTransformer

cstToAst :: TaggedParseTree -> AstNode
cstToAst t = cstToAstTransform transformer t
  where
    transformer = getTransformer t
