module JoosCompiler.Ast.Core where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.SecondaryProcessing.PackageGrouping
import           JoosCompiler.Ast.SecondaryProcessing.ScopeInjection
import           JoosCompiler.Ast.SecondaryProcessing.SubPackaging
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

cstToAst :: TaggedParseTree -> AstNode
cstToAst t = ast2
  where
    ast1 = cstToAstTransform transformer t
    ast2 = injectScopesIntoChildrenBlocks ast1
    transformer = getTransformer t

cstsToAst :: [TaggedParseTree] -> AstNode
cstsToAst ts = grouped
  where
    transformed = map cstToAst ts
    grouped = groupByPackage transformed
    --subPackaged = subPackage grouped

getTransformer :: TaggedParseTree -> Transformer
getTransformer t@(Node label _)
  | (tokenName label) `elem` [kBlock, kConstructorBody, kMethodBody] =
    blockTransformer
  | (tokenName label) `elem` [kClassDeclaration, kInterfaceDeclaration] =
    typeDeclarationTransformer
  | (tokenName label) == kCompilationUnit = compilationUnitTransformer
  | (tokenName label) == kConstructorDeclaration = constructorTransformer
  | (tokenName label) == kFieldDeclaration = fieldTransformer
  | (tokenName label) == kImportDeclaration = importTransformer
  | (tokenName label) == kLocalVariableDeclaration = localVariableTransformer
  | (tokenName label) == kMethodDeclaration = methodTransformer
  | (tokenName label) == kModifier = modifierTransformer
  | (tokenName label) == kModifiers = modifiersTransformer
  | (tokenName label) == kPackageDeclaration = packageTransformer
  | (tokenName label) == kBlockStatement = statementTransformer
  | (tokenName label) `elem` [kType, kVoid] = typeTransformer
  | otherwise = cstTransformer
