module Reachability (checkReturnAndReachability) where

import Data.List
import Data.Maybe
import Data.Tree
import JoosCompiler.Ast
import JoosCompiler.TreeUtils

-- Types

data Annotation = Annotation { isReachable :: Bool
                             , completesNormally :: Bool
                             }

reachableCompletes :: Annotation
reachableCompletes = Annotation True True

type AnnotatedAst = Tree (Annotation, AstWrapper)

-- Logic starts here

-- returns Just x if error, Nothing otherwise
checkReturnAndReachability :: AstNode -> Maybe String
checkReturnAndReachability tree
  | length unreachableNodes > 0 = Just "Found unreachable statements"
  -- TODO add checking for correct return
  | otherwise                   = Nothing
  where
    annotatedTree = annotateTree tree
    unreachableNodes = findChildren ((==) False . isReachable . fst) annotatedTree

annotateTree :: AstNode -> AnnotatedAst
annotateTree tree = applyAnnotation reachableCompletes tree

applyAnnotation :: Annotation -> AstNode -> AnnotatedAst
applyAnnotation parentAnnotation (Node label children) = Node annotatedLabel annotatedChildren
  where
    (_, annotatedChildren) = mapAccumR f parentAnnotation children
    lastChild = last annotatedChildren
    annotatedLabel = (annotation, label)
    annotation = Annotation { isReachable = _isReachable
                            , completesNormally = _completesNormally
                            }
    _isReachable = isReachable parentAnnotation
    _completesNormally
      | length children == 0 = True
      | otherwise            = completesNormally $ fst $ rootLabel lastChild
    f fParentAnnotation fTree = (fAnnotation, fAnnotatedNode)
      where
        fAnnotatedNode = addAnnotation fParentAnnotation fTree
        fAnnotation = fst $ rootLabel fAnnotatedNode

-- Please add rules in the same order as JLS if possible
addAnnotation :: Annotation -> AstNode -> AnnotatedAst
-- A statement can complete normally only if it is reachable
addAnnotation Annotation{isReachable=False, completesNormally=_} tree =
  applyAnnotation (Annotation False False) tree

-- Default case
addAnnotation annotation tree =
  applyAnnotation annotation tree
