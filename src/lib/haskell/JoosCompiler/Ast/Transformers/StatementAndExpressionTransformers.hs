module JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
  ( methodBodyTransformer
  , constructorBodyTransformer
  ) where

import Data.Tree
import Debug.Trace
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Treeify

-- This transformer is different from the rest.
-- It transformers the tree top-down.

asRule :: TaggedParseTree -> [(String, TaggedParseTree)]
asRule n@(Node _ xs) = asPair n : map asPair xs
  where asPair n@(Node x _) = (tokenName x, n)

methodBodyTransformer :: TaggedParseTree -> [Statement]
methodBodyTransformer = match . asRule
  where
    match [("MethodBody", _), ("Block", x)] =
      blockTransformer x
    match [("MethodBody", _), (";", _)] =
      []

blockTransformer :: TaggedParseTree -> [Statement]
blockTransformer = match . asRule
  where
    match [("Block", _), ("{", _), ("}", _)] =
      []
    match [("Block", _), ("{", _), ("BlockStatements", x), ("}", _)] =
      blockStatementsTransformer x

constructorBodyTransformer :: TaggedParseTree -> [Statement]
constructorBodyTransformer = match . asRule
  where
    match [("ConstructorBody", _), ("{", _), ("}", _)] =
      []
    match [("ConstructorBody", _), ("{", _), ("BlockStatements", x), ("}", _)] =
      blockStatementsTransformer x

blockStatementsTransformer :: TaggedParseTree -> [Statement]
blockStatementsTransformer = match . asRule
  where
    match [("BlockStatements", _), ("BlockStatement", x)] =
      [blockStatementTransformer x]
    match [("BlockStatements", _), ("BlockStatements", x), ("BlockStatement", y)] =
      blockStatementsTransformer x ++ [blockStatementTransformer y]

blockStatementTransformer :: TaggedParseTree -> Statement
blockStatementTransformer = match . asRule
  where
    match [("BlockStatement", _), ("LocalVariableDeclarationStatement", x)] =
      emptyScope EmptyStatement --TODO
    match [("BlockStatement", _), ("Statement", x)] =
      statementTransformer x

localVariableDeclarationStatementTransformer :: TaggedParseTree -> [Statement]
localVariableDeclarationStatementTransformer = match . asRule
  where
    match [("LocalVariableDeclarationStatement", _), ("LocalVariableDeclaration", x), (";", _)] =
      [] --TODO

localVariableDeclarationTransformer :: TaggedParseTree -> [Statement]
localVariableDeclarationTransformer = match . asRule
  where
    match [("LocalVariableDeclaration", _), ("Type", _), ("Identifier", _), ("=", _), ("Expression", _)] =
      [] --TODO

statementTransformer :: TaggedParseTree -> Statement
statementTransformer = match . asRule
  where
    match [("Statement", _), ("StatementWithoutTrailingSubstatement", x)] =
      statementWithoutTrailingSubstatementTransformer x
    match [("Statement", _), ("IfThenStatement", x)] =
      ifThenStatementTransformer x
    match [("Statement", _), ("IfThenElseStatement", x)] =
      ifThenElseStatementTransformer x
    match [("Statement", _), ("WhileStatement", x)] =
      whileStatementTransformer x
    match [("Statement", _), ("ForStatement", x)] =
      forStatementTransformer x

statementWithoutTrailingSubstatementTransformer :: TaggedParseTree -> Statement
statementWithoutTrailingSubstatementTransformer = match . asRule
  where
    match [("StatementWithoutTrailingSubstatement", _), ("Block", x)] =
      emptyScope $ BlockStatement $ blockTransformer x
    match [("StatementWithoutTrailingSubstatement", _), ("EmptyStatement", x)] =
      emptyStatementTransformer x
    match [("StatementWithoutTrailingSubstatement", _), ("ExpressionStatement", x)] =
      expressionStatementTransformer x
    match [("StatementWithoutTrailingSubstatement", _), ("ReturnStatement", x)] =
      returnStatementTransformer x

statementNoShortIfTransformer :: TaggedParseTree -> Statement
statementNoShortIfTransformer = match . asRule
  where
    match [("StatementNoShortIf", _), ("StatementWithoutTrailingSubstatement", x)] =
      statementWithoutTrailingSubstatementTransformer x
    match [("StatementNoShortIf", _), ("IfThenElseStatementNoShortIf", x)] =
      ifThenElseStatementNoShortIfTransformer x
    match [("StatementNoShortIf", _), ("WhileStatementNoShortIf", x)] =
      whileStatementNoShortIfTransformer x
    match [("StatementNoShortIf", _), ("ForStatementNoShortIf", x)] =
      forStatementNoShortIfTransformer x

emptyStatementTransformer :: TaggedParseTree -> Statement
emptyStatementTransformer = match . asRule
  where
    match [("EmptyStatement", _), (";", _)] =
      emptyScope EmptyStatement

expressionStatementTransformer :: TaggedParseTree -> Statement
expressionStatementTransformer = match . asRule
  where
    match [("ExpressionStatement", _), ("StatementExpression", x), (";", _)] =
      statementExpressionTransformer x

statementExpressionTransformer :: TaggedParseTree -> Statement
statementExpressionTransformer = match . asRule
  where
    match [("StatementExpression", _), ("Assignment", _)] =
      emptyScope EmptyStatement --TODO
    match [("StatementExpression", _), ("MethodInvocation", _)] =
      emptyScope EmptyStatement --TODO
    match [("StatementExpression", _), ("ClassInstanceCreationExpression", _)] =
      emptyScope EmptyStatement --TODO

ifThenStatementTransformer :: TaggedParseTree -> Statement
ifThenStatementTransformer = match . asRule
  where
    match [("IfThenStatement", _), ("if", _), ("(", _), ("Expression", e), (")", _), ("Statement", s)] =
      emptyScope IfStatement
        { ifPredicate = expressionTransformer e
        , ifThenStatement = statementTransformer s
        , ifElseStatement = emptyScope EmptyStatement }

ifThenElseStatementTransformer :: TaggedParseTree -> Statement
ifThenElseStatementTransformer = match . asRule
  where
    match [("IfThenElseStatement", _), ("if", _), ("(", _), ("Expression", e), (")", _), ("StatementNoShortIf", s1), ("else", _), ("Statement", s2)] =
      emptyScope IfStatement
        { ifPredicate = expressionTransformer e
        , ifThenStatement = statementNoShortIfTransformer s1
        , ifElseStatement = statementTransformer s2 }

ifThenElseStatementNoShortIfTransformer :: TaggedParseTree -> Statement
ifThenElseStatementNoShortIfTransformer = match . asRule
  where
    match [("IfThenElseStatementNoShortIf", _), ("if", _), ("(", _), ("Expression", e), (")", _), ("StatementNoShortIf", s1), ("else", _), ("StatementNoShortIf", s2)] =
      emptyScope IfStatement
       { ifPredicate = expressionTransformer e
       , ifThenStatement = statementNoShortIfTransformer s1
       , ifElseStatement = statementTransformer s2 }

whileStatementTransformer :: TaggedParseTree -> Statement
whileStatementTransformer = match . asRule
  where
    match [("WhileStatement", _), ("while", _), ("(", _), ("Expression", e), (")", _), ("Statement", s)] =
      emptyScope LoopStatement
        { loopPredicate  = expressionTransformer e
        , loopStatements = [statementTransformer s] }

whileStatementNoShortIfTransformer :: TaggedParseTree -> Statement
whileStatementNoShortIfTransformer = match . asRule
  where
    match [("WhileStatementNoShortIf", _), ("while", _), ("(", _), ("Expression", e), (")", _), ("StatementNoShortIf", s)] =
      emptyScope LoopStatement
        { loopPredicate  = expressionTransformer e
        , loopStatements = [statementNoShortIfTransformer s] }

forStatementTransformer :: TaggedParseTree -> Statement
forStatementTransformer = match . asRule
  where
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      emptyScope EmptyStatement --TODO

forStatementNoShortIfTransformer :: TaggedParseTree -> Statement
forStatementNoShortIfTransformer = match . asRule
  where
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope LoopStatement
        { loopPredicate  = emptyType $ Literal Void "true" -- TODO: better literals
        , loopStatements = [statementNoShortIfTransformer x] }
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement -- TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", x)] =
      emptyScope EmptyStatement --TODO

forInitTransformer :: TaggedParseTree -> Statement
forInitTransformer = match . asRule
  where
    match [("ForInit", _), ("StatementExpression", _)] =
      emptyScope EmptyStatement --TODO
    match [("ForInit", _), ("LocalVariableDeclaration", _)] =
      emptyScope EmptyStatement --TODO

forUpdateTransformer :: TaggedParseTree -> Expression
forUpdateTransformer = match . asRule
  where
    match [("ForUpdate", _), ("StatementExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

returnStatementTransformer :: TaggedParseTree -> Statement
returnStatementTransformer = match . asRule
  where
    match [("ReturnStatement", _), ("return", _), (";", _)] =
      emptyScope EmptyStatement --TODO
    match [("ReturnStatement", _), ("return", _), ("Expression", _), (";", _)] =
      emptyScope EmptyStatement --TODO

primaryTransformer :: TaggedParseTree -> Expression
primaryTransformer = match . asRule
  where
    match [("Primary", _), ("PrimaryNoNewArray", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("Primary", _), ("ArrayCreationExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

primaryNoNewArrayTransformer :: TaggedParseTree -> Expression
primaryNoNewArrayTransformer = match . asRule
  where
    match [("PrimaryNoNewArray", _), ("Literal", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("PrimaryNoNewArray", _), ("this", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("PrimaryNoNewArray", _), ("(", _), ("Expression", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("PrimaryNoNewArray", _), ("ClassInstanceCreationExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("PrimaryNoNewArray", _), ("FieldAccess", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("PrimaryNoNewArray", _), ("MethodInvocation", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("PrimaryNoNewArray", _), ("ArrayAccess", _)] =
      emptyType $ Literal Void "TODO" --TODO

classInstanceCreationExpressionTransformer :: TaggedParseTree -> Expression
classInstanceCreationExpressionTransformer = match . asRule
  where
    match [("ClassInstanceCreationExpression", _), ("new", _), ("Name", _), ("(", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ClassInstanceCreationExpression", _), ("new", _), ("Name", _), ("(", _), ("ArgumentList", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO

argumentListTransformer :: TaggedParseTree -> [Expression]
argumentListTransformer = match . asRule
  where
    match [("ArgumentList", _), ("Expression", _)] =
      [emptyType $ Literal Void "TODO"] --TODO
    match [("ArgumentList", _), ("ArgumentList", _), (",", _), ("Expression", _)] =
      [emptyType $ Literal Void "TODO"] --TODO

arrayCreationExpressionTransformer :: TaggedParseTree -> Expression
arrayCreationExpressionTransformer = match . asRule
  where
    match [("ArrayCreationExpression", _), ("new", _), ("Name", _), ("[", _), ("]", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ArrayCreationExpression", _), ("new", _), ("Name", _), ("[", _), ("Expression", _), ("]", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ArrayCreationExpression", _), ("new", _), ("PrimitiveType", _), ("[", _), ("]", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ArrayCreationExpression", _), ("new", _), ("PrimitiveType", _), ("[", _), ("Expression", _), ("]", _)] =
      emptyType $ Literal Void "TODO" --TODO

fieldAccessTransformer :: TaggedParseTree -> Expression
fieldAccessTransformer = match . asRule
  where
    match [("FieldAccess", _), ("Primary", _), (".", _), ("Identifier", _)] =
      emptyType $ Literal Void "TODO" --TODO

methodInvocationTransformer :: TaggedParseTree -> Expression
methodInvocationTransformer = match . asRule
  where
    match [("MethodInvocation", _), ("Name", _), ("(", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("MethodInvocation", _), ("Name", _), ("(", _), ("ArgumentList", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("MethodInvocation", _), ("Primary", _), (".", _), ("Identifier", _), ("(", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("MethodInvocation", _), ("Primary", _), (".", _), ("Identifier", _), ("(", _), ("ArgumentList", _), (")", _)] =
      emptyType $ Literal Void "TODO" --TODO

arrayAccessTransformer :: TaggedParseTree -> Expression
arrayAccessTransformer = match . asRule
  where
    match [("ArrayAccess", _), ("Name", _), ("[", _), ("Expression", _), ("]", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ArrayAccess", _), ("PrimaryNoNewArray", _), ("[", _), ("Expression", _), ("]", _)] =
      emptyType $ Literal Void "TODO" --TODO

unaryExpressionTransformer :: TaggedParseTree -> Expression
unaryExpressionTransformer = match . asRule
  where
    match [("UnaryExpression", _), ("-", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("UnaryExpression", _), ("UnaryExpressionNotPlusMinus", _)] =
      emptyType $ Literal Void "TODO" --TODO

unaryExpressionNotPlusMinusTransformer :: TaggedParseTree -> Expression
unaryExpressionNotPlusMinusTransformer = match . asRule
  where
    match [("UnaryExpressionNotPlusMinus", _), ("Primary", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("UnaryExpressionNotPlusMinus", _), ("Name", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("UnaryExpressionNotPlusMinus", _), ("!", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("UnaryExpressionNotPlusMinus", _), ("CastExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

caseExpressionTransformer :: TaggedParseTree -> Expression
caseExpressionTransformer = match . asRule
  where
    match [("CastExpression", _), ("(", _), ("PrimitiveType", _), (")", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("CastExpression", _), ("(", _), ("PrimitiveType", _), ("[", _), ("]", _), (")", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("CastExpression", _), ("(", _), ("Expression", _), (")", _), ("UnaryExpressionNotPlusMinus", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("CastExpression", _), ("(", _), ("Name", _), ("[", _), ("]", _), (")", _), ("UnaryExpressionNotPlusMinus", _)] =
      emptyType $ Literal Void "TODO" --TODO

multiplicativeExpressionTransformer :: TaggedParseTree -> Expression
multiplicativeExpressionTransformer = match . asRule
  where
    match [("MultiplicativeExpression", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", _), ("*", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", _), ("/", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", _), ("%", _), ("UnaryExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

additiveExpressionTransformer :: TaggedParseTree -> Expression
additiveExpressionTransformer = match . asRule
  where
    match [("AdditiveExpression", _), ("MultiplicativeExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("AdditiveExpression", _), ("AdditiveExpression", _), ("+", _), ("MultiplicativeExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("AdditiveExpression", _), ("AdditiveExpression", _), ("-", _), ("MultiplicativeExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

relationalExpressionTransformer :: TaggedParseTree -> Expression
relationalExpressionTransformer = match . asRule
  where
    match [("RelationalExpression", _), ("AdditiveExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), ("<", _), ("AdditiveExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), (">", _), ("AdditiveExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), ("<=", _), ("AdditiveExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), (">=", _), ("AdditiveExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), ("instanceof", _), ("ReferenceType", _)] =
      emptyType $ Literal Void "TODO" --TODO

equalityExpressionTransformer :: TaggedParseTree -> Expression
equalityExpressionTransformer = match . asRule
  where
    match [("EqualityExpression", _), ("RelationalExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("EqualityExpression", _), ("EqualityExpression", _), ("==", _), ("RelationalExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("EqualityExpression", _), ("EqualityExpression", _), ("!=", _), ("RelationalExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

andExpressionTransformer :: TaggedParseTree -> Expression
andExpressionTransformer = match . asRule
  where
    match [("AndExpression", _), ("EqualityExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("AndExpression", _), ("AndExpression", _), ("&", _), ("EqualityExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

inclusiveOrExpressionTransformer :: TaggedParseTree -> Expression
inclusiveOrExpressionTransformer = match . asRule
  where
    match [("InclusiveOrExpression", _), ("AndExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("InclusiveOrExpression", _), ("InclusiveOrExpression", _), ("|", _), ("AndExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

conditionalAndExpressionTransformer :: TaggedParseTree -> Expression
conditionalAndExpressionTransformer = match . asRule
  where
    match [("ConditionalAndExpression", _), ("InclusiveOrExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ConditionalAndExpression", _), ("ConditionalAndExpression", _), ("&&", _), ("InclusiveOrExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

conditionalOrExpressionTransformer :: TaggedParseTree -> Expression
conditionalOrExpressionTransformer = match . asRule
  where
    match [("ConditionalOrExpression", _), ("ConditionalAndExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("ConditionalOrExpression", _), ("ConditionalOrExpression", _), ("||", _), ("ConditionalAndExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

assignmentExpressionTransformer :: TaggedParseTree -> Expression
assignmentExpressionTransformer = match . asRule
  where
    match [("AssignmentExpression", _), ("Assignment", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("AssignmentExpression", _), ("ConditionalOrExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

assignmentTransformer :: TaggedParseTree -> Expression
assignmentTransformer = match . asRule
  where
    match [("Assignment", _), ("Name", _), ("=", _), ("AssignmentExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("Assignment", _), ("FieldAccess", _), ("=", _), ("AssignmentExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("Assignment", _), ("ArrayAccess", _), ("=", _), ("AssignmentExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO

expressionTransformer :: TaggedParseTree -> Expression
expressionTransformer = match . asRule
  where
    match [("Expression", _), ("AssignmentExpression", _)] =
      emptyType $ Literal Void "TODO" --TODO
