module JoosCompiler.Ast.Core(cstsToAst) where

import           Data.Tree
import           Flow
import           JoosCompiler.Ast.SecondaryProcessing.StatementBlocks
import           JoosCompiler.Ast.SecondaryProcessing.Packaging
import           JoosCompiler.Ast.SecondaryProcessing.ScopeInjection
import           JoosCompiler.Ast.SecondaryProcessing.TypeCanonicalization
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
cstToAst t = ast
  where
    ast = cstToAstTransform transformer t
    transformer = getTransformer t

cstsToAst :: [TaggedParseTree] -> AstNode
cstsToAst ts = program
  where
    transformedCompilationUnits = map cstToAst ts
    program =
      transformedCompilationUnits
      |> packageProgram
      |> canonicalizeProgram
      -- Break program and repackage once again so that compilation units in
      -- WholeProgram are updated
      |> subForest
      |> packageProgram
      -- TODO this is disabled so hierarchy checking works
      -- |> asAst
      -- |> asTree
      |> insertBlocksAroundStatements
      |> injectScopesIntoChildrenBlocks

getTransformer :: TaggedParseTree -> Transformer
getTransformer t@(Node label _)
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
  | otherwise = cstTransformer
