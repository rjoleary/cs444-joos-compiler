module JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
  ( methodBodyTransformer
  , constructorBodyTransformer
  ) where

import Data.Tree
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
      [] -- TODO
    match [("Block", _), ("{", _), ("BlockStatements", x), ("}", _)] =
      [] -- TODO

constructorBodyTransformer :: TaggedParseTree -> [Statement]
constructorBodyTransformer = match . asRule
  where
    match [("ConstructorBody", _), ("{", _), ("}", _)] =
      [] -- TODO
    match [("ConstructorBody", _), ("{", _), ("BlockStatements", _), ("}", _)] =
      [] -- TODO

blockStatementsTransformer :: TaggedParseTree -> [Statement]
blockStatementsTransformer = match . asRule
  where
    match [("BlockStatements", _), ("BlockStatement", x)] =
      [] --TODO
    match [("BlockStatements", _), ("BlockStatements", x), ("BlockStatement", y)] =
      [] --TODO

blockStatementTransformer :: TaggedParseTree -> [Statement]
blockStatementTransformer = match . asRule
  where
    match [("BlockStatement", _), ("LocalVariableDeclarationStatement", x)] =
      [] --TODO
    match [("BlockStatement", _), ("Statement", x)] =
      [] --TODO

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

statementTransformer :: TaggedParseTree -> [Statement]
statementTransformer = match . asRule
  where
    match [("Statement", _), ("StatementWithoutTrailingSubstatement", _)] =
      [] --TODO
    match [("Statement", _), ("IfThenStatement", _)] =
      [] --TODO
    match [("Statement", _), ("IfThenElseStatement", _)] =
      [] --TODO
    match [("Statement", _), ("WhileStatement", _)] =
      [] --TODO
    match [("Statement", _), ("ForStatement", _)] =
      [] --TODO

statementWithoutTrailingSubstatementTransformer :: TaggedParseTree -> [Statement]
statementWithoutTrailingSubstatementTransformer = match . asRule
  where
    match [("StatementWithoutTrailingSubstatement", _), ("Block", _)] =
      [] --TODO
    match [("StatementWithoutTrailingSubstatement", _), ("EmptyStatement", _)] =
      [] --TODO
    match [("StatementWithoutTrailingSubstatement", _), ("ExpressionStatement", _)] =
      [] --TODO
    match [("StatementWithoutTrailingSubstatement", _), ("ReturnStatement", _)] =
      [] --TODO

statementNoShortIfTransformer :: TaggedParseTree -> [Statement]
statementNoShortIfTransformer = match . asRule
  where
    match [("StatementNoShortIf", _), ("StatementWithoutTrailingSubstatement", _)] =
      [] --TODO
    match [("StatementNoShortIf", _), ("IfThenElseStatementNoShortIf", _)] =
      [] --TODO
    match [("StatementNoShortIf", _), ("WhileStatementNoShortIf", _)] =
      [] --TODO
    match [("StatementNoShortIf", _), ("ForStatementNoShortIf", _)] =
      [] --TODO

emptyStatementTransformer :: TaggedParseTree -> [Statement]
emptyStatementTransformer = match . asRule
  where
    match [("EmptyStatement", _), (";", _)] =
      [] --TODO

expressionStatementTransformer :: TaggedParseTree -> [Statement]
expressionStatementTransformer = match . asRule
  where
    match [("ExpressionStatement", _), ("StatementExpression", _), (";", _)] =
      [] --TODO

statementExpressionTransformer :: TaggedParseTree -> [Statement]
statementExpressionTransformer = match . asRule
  where
    match [("StatementExpression", _), ("Assignment", _)] =
      [] --TODO
    match [("StatementExpression", _), ("MethodInvocation", _)] =
      [] --TODO
    match [("StatementExpression", _), ("ClassInstanceCreationExpression", _)] =
      [] --TODO

ifThenStatementTransformer :: TaggedParseTree -> [Statement]
ifThenStatementTransformer = match . asRule
  where
    match [("IfThenStatement", _), ("if", _), ("(", _), ("Expression", _), (")", _), ("Statement", _)] =
      [] --TODO

ifThenElseStatementTransformer :: TaggedParseTree -> [Statement]
ifThenElseStatementTransformer = match . asRule
  where
    match [("IfThenElseStatement", _), ("if", _), ("(", _), ("Expression", _), (")", _), ("StatementNoShortIf", _), ("else", _), ("Statement", _)] =
      [] --TODO

ifThenElseStatementNoShortIfTransformer :: TaggedParseTree -> [Statement]
ifThenElseStatementNoShortIfTransformer = match . asRule
  where
    match [("IfThenElseStatementNoShortIf", _), ("if", _), ("(", _), ("Expression", _), (")", _), ("StatementNoShortIf", _), ("else", _), ("StatementNoShortIf", _)] =
      [] --TODO

whileStatementTransformer :: TaggedParseTree -> [Statement]
whileStatementTransformer = match . asRule
  where
    match [("WhileStatement", _), ("while", _), ("(", _), ("Expression", _), (")", _), ("Statement", _)] =
      [] --TODO

whileStatementNoShortIfTransformer :: TaggedParseTree -> [Statement]
whileStatementNoShortIfTransformer = match . asRule
  where
    match [("WhileStatementNoShortIf", _), ("while", _), ("(", _), ("Expression", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO

forStatementTransformer :: TaggedParseTree -> [Statement]
forStatementTransformer = match . asRule
  where
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      [] --TODO
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("Statement", _)] =
      [] --TODO

forStatementNoShortIfTransformer :: TaggedParseTree -> [Statement]
forStatementNoShortIfTransformer = match . asRule
  where
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", _), (";", _), ("Expression", _), (";", _), ("ForUpdate", _), (")", _), ("StatementNoShortIf", _)] =
      [] --TODO

forInitTransformer :: TaggedParseTree -> [Statement]
forInitTransformer = match . asRule
  where
    match [("ForInit", _), ("StatementExpression", _)] =
      [] --TODO
    match [("ForInit", _), ("LocalVariableDeclaration", _)] =
      [] --TODO

forUpdateTransformer :: TaggedParseTree -> [Statement]
forUpdateTransformer = match . asRule
  where
    match [("ForUpdate", _), ("StatementExpression", _)] =
      [] --TODO

returnStatementTransformer :: TaggedParseTree -> [Statement]
returnStatementTransformer = match . asRule
  where
    match [("ReturnStatement", _), ("return", _), (";", _)] =
      [] --TODO
    match [("ReturnStatement", _), ("return", _), ("Expression", _), (";", _)] =
      [] --TODO

primaryTransformer :: TaggedParseTree -> [Statement]
primaryTransformer = match . asRule
  where
    match [("Primary", _), ("PrimaryNoNewArray", _)] =
      [] --TODO
    match [("Primary", _), ("ArrayCreationExpression", _)] =
      [] --TODO

primaryNoNewArrayTransformer :: TaggedParseTree -> [Statement]
primaryNoNewArrayTransformer = match . asRule
  where
    match [("PrimaryNoNewArray", _), ("Literal", _)] =
      [] --TODO
    match [("PrimaryNoNewArray", _), ("this", _)] =
      [] --TODO
    match [("PrimaryNoNewArray", _), ("(", _), ("Expression", _), (")", _)] =
      [] --TODO
    match [("PrimaryNoNewArray", _), ("ClassInstanceCreationExpression", _)] =
      [] --TODO
    match [("PrimaryNoNewArray", _), ("FieldAccess", _)] =
      [] --TODO
    match [("PrimaryNoNewArray", _), ("MethodInvocation", _)] =
      [] --TODO
    match [("PrimaryNoNewArray", _), ("ArrayAccess", _)] =
      [] --TODO

classInstanceCreationExpressionTransformer :: TaggedParseTree -> [Statement]
classInstanceCreationExpressionTransformer = match . asRule
  where
    match [("ClassInstanceCreationExpression", _), ("new", _), ("Name", _), ("(", _), (")", _)] =
      [] --TODO
    match [("ClassInstanceCreationExpression", _), ("new", _), ("Name", _), ("(", _), ("ArgumentList", _), (")", _)] =
      [] --TODO

argumentListTransformer :: TaggedParseTree -> [Statement]
argumentListTransformer = match . asRule
  where
    match [("ArgumentList", _), ("Expression", _)] =
      [] --TODO
    match [("ArgumentList", _), ("ArgumentList", _), (",", _), ("Expression", _)] =
      [] --TODO

arrayCreationExpressionTransformer :: TaggedParseTree -> [Statement]
arrayCreationExpressionTransformer = match . asRule
  where
    match [("ArrayCreationExpression", _), ("new", _), ("Name", _), ("[", _), ("]", _)] =
      [] --TODO
    match [("ArrayCreationExpression", _), ("new", _), ("Name", _), ("[", _), ("Expression", _), ("]", _)] =
      [] --TODO
    match [("ArrayCreationExpression", _), ("new", _), ("PrimitiveType", _), ("[", _), ("]", _)] =
      [] --TODO
    match [("ArrayCreationExpression", _), ("new", _), ("PrimitiveType", _), ("[", _), ("Expression", _), ("]", _)] =
      [] --TODO

fieldAccessTransformer :: TaggedParseTree -> [Statement]
fieldAccessTransformer = match . asRule
  where
    match [("FieldAccess", _), ("Primary", _), (".", _), ("Identifier", _)] =
      [] --TODO

methodInvocationTransformer :: TaggedParseTree -> [Statement]
methodInvocationTransformer = match . asRule
  where
    match [("MethodInvocation", _), ("Name", _), ("(", _), (")", _)] =
      [] --TODO
    match [("MethodInvocation", _), ("Name", _), ("(", _), ("ArgumentList", _), (")", _)] =
      [] --TODO
    match [("MethodInvocation", _), ("Primary", _), (".", _), ("Identifier", _), ("(", _), (")", _)] =
      [] --TODO
    match [("MethodInvocation", _), ("Primary", _), (".", _), ("Identifier", _), ("(", _), ("ArgumentList", _), (")", _)] =
      [] --TODO

arrayAccessTransformer :: TaggedParseTree -> [Statement]
arrayAccessTransformer = match . asRule
  where
    match [("ArrayAccess", _), ("Name", _), ("[", _), ("Expression", _), ("]", _)] =
      [] --TODO
    match [("ArrayAccess", _), ("PrimaryNoNewArray", _), ("[", _), ("Expression", _), ("]", _)] =
      [] --TODO

unaryExpressionTransformer :: TaggedParseTree -> [Statement]
unaryExpressionTransformer = match . asRule
  where
    match [("UnaryExpression", _), ("-", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("UnaryExpression", _), ("UnaryExpressionNotPlusMinus", _)] =
      [] --TODO

unaryExpressionNotPlusMinusTransformer :: TaggedParseTree -> [Statement]
unaryExpressionNotPlusMinusTransformer = match . asRule
  where
    match [("UnaryExpressionNotPlusMinus", _), ("Primary", _)] =
      [] --TODO
    match [("UnaryExpressionNotPlusMinus", _), ("Name", _)] =
      [] --TODO
    match [("UnaryExpressionNotPlusMinus", _), ("!", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("UnaryExpressionNotPlusMinus", _), ("CastExpression", _)] =
      [] --TODO

caseExpressionTransformer :: TaggedParseTree -> [Statement]
caseExpressionTransformer = match . asRule
  where
    match [("CastExpression", _), ("(", _), ("PrimitiveType", _), (")", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("CastExpression", _), ("(", _), ("PrimitiveType", _), ("[", _), ("]", _), (")", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("CastExpression", _), ("(", _), ("Expression", _), (")", _), ("UnaryExpressionNotPlusMinus", _)] =
      [] --TODO
    match [("CastExpression", _), ("(", _), ("Name", _), ("[", _), ("]", _), (")", _), ("UnaryExpressionNotPlusMinus", _)] =
      [] --TODO

multiplicativeExpressionTransformer :: TaggedParseTree -> [Statement]
multiplicativeExpressionTransformer = match . asRule
  where
    match [("MultiplicativeExpression", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", _), ("*", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", _), ("/", _), ("UnaryExpression", _)] =
      [] --TODO
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", _), ("%", _), ("UnaryExpression", _)] =
      [] --TODO

additiveExpressionTransformer :: TaggedParseTree -> [Statement]
additiveExpressionTransformer = match . asRule
  where
    match [("AdditiveExpression", _), ("MultiplicativeExpression", _)] =
      [] --TODO
    match [("AdditiveExpression", _), ("AdditiveExpression", _), ("+", _), ("MultiplicativeExpression", _)] =
      [] --TODO
    match [("AdditiveExpression", _), ("AdditiveExpression", _), ("-", _), ("MultiplicativeExpression", _)] =
      [] --TODO

relationalExpressionTransformer :: TaggedParseTree -> [Statement]
relationalExpressionTransformer = match . asRule
  where
    match [("RelationalExpression", _), ("AdditiveExpression", _)] =
      [] --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), ("<", _), ("AdditiveExpression", _)] =
      [] --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), (">", _), ("AdditiveExpression", _)] =
      [] --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), ("<=", _), ("AdditiveExpression", _)] =
      [] --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), (">=", _), ("AdditiveExpression", _)] =
      [] --TODO
    match [("RelationalExpression", _), ("RelationalExpression", _), ("instanceof", _), ("ReferenceType", _)] =
      [] --TODO

equalityExpressionTransformer :: TaggedParseTree -> [Statement]
equalityExpressionTransformer = match . asRule
  where
    match [("EqualityExpression", _), ("RelationalExpression", _)] =
      [] --TODO
    match [("EqualityExpression", _), ("EqualityExpression", _), ("==", _), ("RelationalExpression", _)] =
      [] --TODO
    match [("EqualityExpression", _), ("EqualityExpression", _), ("!=", _), ("RelationalExpression", _)] =
      [] --TODO

andExpressionTransformer :: TaggedParseTree -> [Statement]
andExpressionTransformer = match . asRule
  where
    match [("AndExpression", _), ("EqualityExpression", _)] =
      [] --TODO
    match [("AndExpression", _), ("AndExpression", _), ("&", _), ("EqualityExpression", _)] =
      [] --TODO

inclusiveOrExpressionTransformer :: TaggedParseTree -> [Statement]
inclusiveOrExpressionTransformer = match . asRule
  where
    match [("InclusiveOrExpression", _), ("AndExpression", _)] =
      [] --TODO
    match [("InclusiveOrExpression", _), ("InclusiveOrExpression", _), ("|", _), ("AndExpression", _)] =
      [] --TODO

conditionalAndExpressionTransformer :: TaggedParseTree -> [Statement]
conditionalAndExpressionTransformer = match . asRule
  where
    match [("ConditionalAndExpression", _), ("InclusiveOrExpression", _)] =
      [] --TODO
    match [("ConditionalAndExpression", _), ("ConditionalAndExpression", _), ("&&", _), ("InclusiveOrExpression", _)] =
      [] --TODO

conditionalOrExpressionTransformer :: TaggedParseTree -> [Statement]
conditionalOrExpressionTransformer = match . asRule
  where
    match [("ConditionalOrExpression", _), ("ConditionalAndExpression", _)] =
      [] --TODO
    match [("ConditionalOrExpression", _), ("ConditionalOrExpression", _), ("||", _), ("ConditionalAndExpression", _)] =
      [] --TODO

assignmentExpressionTransformer :: TaggedParseTree -> [Statement]
assignmentExpressionTransformer = match . asRule
  where
    match [("AssignmentExpression", _), ("Assignment", _)] =
      [] --TODO
    match [("AssignmentExpression", _), ("ConditionalOrExpression", _)] =
      [] --TODO

assignmentTransformer :: TaggedParseTree -> [Statement]
assignmentTransformer = match . asRule
  where
    match [("Assignment", _), ("Name", _), ("=", _), ("AssignmentExpression", _)] =
      [] --TODO
    match [("Assignment", _), ("FieldAccess", _), ("=", _), ("AssignmentExpression", _)] =
      [] --TODO
    match [("Assignment", _), ("ArrayAccess", _), ("=", _), ("AssignmentExpression", _)] =
      [] --TODO

expressionTransformer :: TaggedParseTree -> [Statement]
expressionTransformer = match . asRule
  where
    match [("Expression", _), ("AssignmentExpression", _)] =
      [] --TODO
