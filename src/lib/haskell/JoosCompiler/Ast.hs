module JoosCompiler.Ast where

import JoosCompiler.Ast.NodeTypes
import Data.Tree
import JoosCompiler.Treeify
import JoosCompiler.Ast.Transformers.Types

import JoosCompiler.Ast.Transformers

-- We first transform children so root has access to them
cstToAstTransform :: Transformer -> TaggedParseTree -> AstNode
cstToAstTransform f t = Node transformedRoot transformedChildren
  where
    transformedChildren = map cstToAst $ subForest t
    transformedRoot = f transformedChildren t

getTransformer :: TaggedParseTree -> Transformer
getTransformer t@(Node label _)
  | (tokenName label) `elem` [kClassDeclaration, kInterfaceDeclaration] =
    classDeclarationTransformer
  | otherwise = cstTransformer

cstToAst :: TaggedParseTree -> AstNode
cstToAst t = cstToAstTransform transformer t
  where transformer = getTransformer t
