module JoosCompiler.Ast.SecondaryProcessing.Disambiguation
  (disambiguate) where

import           Data.Maybe
import           Data.Tree
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types

{- TODO
    - traverse tree until we see expression name
    - if expression name, try to resolve first part as local
    - otherwise,          try to resolve as field (double check the order)
    - otherwise,          try to resolve as an import
    - otherwise,          try to resolve as a package
      in this case, we keep resolving as a package until we hit a type

   Once it's resolved, we recursively resolve the rest of the name. Needs to be
   a type name in order for the resolution to be valid. We then generate a tree
   of "accesses" (staticFieldAccess, fieldAccess, localAccess)
-}

disambiguate :: AstNode -> AstNode
disambiguate (Node p@(AstWholeProgram program) units) =
  Node p $ map (disambiguateUnit program) units
disambiguate _ = error "Invalid node type in disambiguate"

disambiguateUnit :: WholeProgram -> AstNode -> AstNode
disambiguateUnit program t@(Node (AstCompilationUnit oldUnit) children)
  | maybeOldTypeDecl == Nothing = t
  | otherwise = Node (AstCompilationUnit newUnit) newChildren
  where
    maybeOldTypeDecl = typeDecl oldUnit
    oldTypeDecl = fromMaybe (error "oldTypeDecl was Nothing") maybeOldTypeDecl
    newTypeDecl = oldTypeDecl {methods = newMethods}
    newUnit = oldUnit {typeDecl = Just newTypeDecl}
    newMethods = map (disambiguateMethod program) $ methods oldTypeDecl
    newChildren = map (disambiguateTree program) children
disambiguateUnit _ _ = error "Wrong node type in disambiguateUnit"

disambiguateMethod :: WholeProgram -> Method -> Method
disambiguateMethod program method =
  method { methodStatement = newStatement }
  where
    oldStatement = methodStatement method
    newStatement = disambiguateStatement program oldStatement

disambiguateStatement :: WholeProgram -> Statement -> Statement
disambiguateStatement program statement = newStatement
  where
    newStatement = mapStatementExpression f statement
    f = id

disambiguateTree :: WholeProgram -> AstNode -> AstNode
disambiguateTree _ = id
