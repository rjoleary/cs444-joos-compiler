{-# LANGUAGE MultiParamTypeClasses #-}
module StaticAnalysis.Reachability
  ( checkReturnAndReachability
  , checkReachability ) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Tree
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Visitor.Analysis
import JoosCompiler.Error
import JoosCompiler.TreeUtils
import StaticAnalysis.DefiniteAssignment

data Status = CompletesNormally | CompletesAbnormally deriving (Eq, Show)

instance Monoid Status where
  mempty = CompletesNormally
  mappend CompletesAbnormally _ = CompletesAbnormally
  mappend _ CompletesAbnormally = CompletesAbnormally
  mappend _ _                   = CompletesNormally

checkReachability :: AstNode -> Either String ()
checkReachability (Node (AstWholeProgram wp) _) =
  fmap (const ()) $ (analyzeWholeProgram CheckReachability wp :: Either String Status)

data CheckReachability = CheckReachability

instance Analysis CheckReachability Status where
  analyzeMethod ctx m@Method{methodReturn=r, methodStatement=s} =
    if s == TerminalStatement
      then Right CompletesNormally -- Ignore methods with not implementation.
      else do
        status <- analyzeStatement ctx s
        if r /= Void && status == CompletesNormally
          then Left ("Non-void method '" ++ methodName m ++ "' must return on all paths")
          else return status

  -- A while loop condition must not evaluate to true.
  analyzeStatement ctx s@LoopStatement{loopPredicate=e, nextStatement=n}
    | evalExpr e == ConstBool True && n /= TerminalStatement =
      Left "Statements may not proceed a loop which does not complete normally"
    | evalExpr e == ConstBool False =
      Left "A loop condition must not always evaluate to false"
    | evalExpr e == ConstBool True =
      Right CompletesAbnormally
    | otherwise = analyzeStatement ctx n

  analyzeStatement ctx s@IfStatement{} = do
    ifThenStatus <- analyzeStatement ctx $ ifThenStatement s
    ifElseStatus <- analyzeStatement ctx $ ifElseStatement s
    nextStatus <- analyzeStatement ctx $ nextStatement s
    if ifThenStatus == CompletesAbnormally && ifElseStatus == CompletesAbnormally
    then
      if nextStatement s == TerminalStatement
      then return CompletesAbnormally
      else Left "Statements may not proceed an if-statement which does not complete normally"
    else return nextStatus

  -- No statement is allowed after the return statement.
  analyzeStatement ctx ReturnStatement{nextStatement=TerminalStatement} =
    Right CompletesAbnormally
  analyzeStatement ctx ReturnStatement{nextStatement=_} =
    Left "No statement allowed after the return statement"

  -- All other statements complete normally.
  analyzeStatement ctx s = analyzeStatement (DefaultAnalysis ctx) s



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

-- A statement is reachable iff the statement preceding it completes normally
addAnnotation Annotation{isReachable=True, completesNormally=False} tree =
  applyAnnotation (Annotation False False) tree

-- Implicit rule: A local variable declaration can complete normally iff it is
-- reachable

-- Implicit: empty statement

-- TODO: a labeled statement can complete normally if at least one of the
-- following is true:
--     - the contained statement can complete normally
--     - there is a reachable break that exits the labelled statement
-- Implicit: contained statement reachable iff labelled statement is reachable

-- Implicit: Expression statement completes normally iff it is reachable

-- TODO: a while statement completes normally if at least one of the following
-- is true
--     - The condition expression is not `true`
--     - There is a reachable `break` statement that exits the while loop
--
-- Note: we can simply check that there is a break statement. If the break statement
-- is unreachable, that would cause a reachability error
--
-- Contained statement reachable iff while statement reachable (implicit) and
-- the condition is not `false`

-- We will probably convert for loops to while loops, so nothing to do for for loops

-- TODO: break, continue, return do not complete normally

-- TODO if-then statement can complete normally if it is reachable
-- Implicit: the then-statement is reachable if the if-then is reachable

-- TODO if-then-else statement can complete normally if the then-statement can
-- complete normally or the else-statement can complete normally.
-- Implicit: the then-statement, else-statement are reachable if the
-- if-then-else is reachable

-- Default case
addAnnotation annotation tree =
  applyAnnotation annotation tree
