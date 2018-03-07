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

nameTransformer :: TaggedParseTree -> Name
nameTransformer = match . asRule
  where
    match [("Name", _), ("Identifier", x)] =
      [tokenString $ lhs x]
    match [("Name", _), ("Name", xs), (".", _), ("Identifier", x)] =
      nameTransformer xs ++ [tokenString $ lhs x]

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

localVariableDeclarationStatementTransformer :: TaggedParseTree -> Statement
localVariableDeclarationStatementTransformer = match . asRule
  where
    match [("LocalVariableDeclarationStatement", _), ("LocalVariableDeclaration", x), (";", _)] =
      emptyScope EmptyStatement --TODO

localVariableDeclarationTransformer :: TaggedParseTree -> Statement
localVariableDeclarationTransformer = match . asRule
  where
    match [("LocalVariableDeclaration", _), ("Type", t), ("Identifier", n), ("=", _), ("Expression", x)] =
      emptyScope EmptyStatement -- TODO
      --Local
      --{ localType      = Void -- TODO
      --, localModifiers = [] -- TODO: why does this field exist?
      --, localName      = tokenString $ lhs n
      --, localValue     = expressionTransformer x
      --}

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
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("Statement", x)] =
      genericForLoop (emptyScope EmptyStatement) (emptyType $ Literal Void "true") (emptyScope EmptyStatement) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (emptyType $ Literal Void "true") (emptyScope EmptyStatement) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), (")", _), ("Statement", x)] =
      genericForLoop (emptyScope EmptyStatement) (expressionTransformer e) (emptyScope EmptyStatement) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) (emptyScope EmptyStatement) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop (emptyScope EmptyStatement) (emptyType $ Literal Void "true") (forUpdateTransformer s2) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (emptyType $ Literal Void "true") (forUpdateTransformer s2) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop (emptyScope EmptyStatement) (expressionTransformer e) (forUpdateTransformer s2) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) (forUpdateTransformer s2) (statementTransformer x)

forStatementNoShortIfTransformer :: TaggedParseTree -> Statement
forStatementNoShortIfTransformer = match . asRule
  where
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (emptyScope EmptyStatement) (emptyType $ Literal Void "true") (emptyScope EmptyStatement) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (emptyType $ Literal Void "true") (emptyScope EmptyStatement) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (emptyScope EmptyStatement) (expressionTransformer e) (emptyScope EmptyStatement) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) (emptyScope EmptyStatement) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (emptyScope EmptyStatement) (emptyType $ Literal Void "true") (forUpdateTransformer s2) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (emptyType $ Literal Void "true") (forUpdateTransformer s2) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (emptyScope EmptyStatement) (expressionTransformer e) (forUpdateTransformer s2) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) (forUpdateTransformer s2) (statementNoShortIfTransformer x)

-- Helper function for for-statements
genericForLoop init expr update statement =
  emptyScope BlockStatement
    { blockStatements =
      [ init
      , emptyScope LoopStatement
        { loopPredicate = expr
        , loopStatements =
          [ emptyScope BlockStatement
            { blockStatements = [statement] }
          , statement ] } ] }

forInitTransformer :: TaggedParseTree -> Statement
forInitTransformer = match . asRule
  where
    match [("ForInit", _), ("StatementExpression", x)] =
      statementExpressionTransformer x
    match [("ForInit", _), ("LocalVariableDeclaration", x)] =
      localVariableDeclarationTransformer x

forUpdateTransformer :: TaggedParseTree -> Statement
forUpdateTransformer = match . asRule
  where
    match [("ForUpdate", _), ("StatementExpression", x)] =
      statementExpressionTransformer x

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
    match [("PrimaryNoNewArray", _), ("Literal", x)] =
      emptyType $ Literal Void (tokenString $ lhs x)
    match [("PrimaryNoNewArray", _), ("this", _)] =
      emptyType This
    match [("PrimaryNoNewArray", _), ("(", _), ("Expression", x), (")", _)] =
      expressionTransformer x
    match [("PrimaryNoNewArray", _), ("ClassInstanceCreationExpression", x)] =
      classInstanceCreationExpressionTransformer x
    match [("PrimaryNoNewArray", _), ("FieldAccess", x)] =
      fieldAccessTransformer x
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
    match [("FieldAccess", _), ("Primary", x), (".", _), ("Identifier", y)] =
      emptyType $ FieldAccess (primaryTransformer x) (tokenString $ lhs y)

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
    match [("UnaryExpression", _), ("UnaryExpressionNotPlusMinus", x)] =
      unaryExpressionNotPlusMinusTransformer x

unaryExpressionNotPlusMinusTransformer :: TaggedParseTree -> Expression
unaryExpressionNotPlusMinusTransformer = match . asRule
  where
    match [("UnaryExpressionNotPlusMinus", _), ("Primary", x)] =
      primaryTransformer x
    match [("UnaryExpressionNotPlusMinus", _), ("Name", x)] =
      emptyType $ ExpressionName (nameTransformer x)
    match [("UnaryExpressionNotPlusMinus", _), ("!", _), ("UnaryExpression", x)] =
      emptyType $ UnaryOperation Negate (unaryExpressionTransformer x)
    match [("UnaryExpressionNotPlusMinus", _), ("CastExpression", x)] =
      castExpressionTransformer x

castExpressionTransformer :: TaggedParseTree -> Expression
castExpressionTransformer = match . asRule
  where
    match [("CastExpression", _), ("(", _), ("PrimitiveType", t), (")", _), ("UnaryExpression", x)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("CastExpression", _), ("(", _), ("PrimitiveType", t), ("[", _), ("]", _), (")", _), ("UnaryExpression", x)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("CastExpression", _), ("(", _), ("Expression", t), (")", _), ("UnaryExpressionNotPlusMinus", x)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("CastExpression", _), ("(", _), ("Name", t), ("[", _), ("]", _), (")", _), ("UnaryExpressionNotPlusMinus", x)] =
      emptyType $ Literal Void "TODO" --TODO

multiplicativeExpressionTransformer :: TaggedParseTree -> Expression
multiplicativeExpressionTransformer = match . asRule
  where
    match [("MultiplicativeExpression", _), ("UnaryExpression", x)] =
      unaryExpressionTransformer x
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", x), ("*", _), ("UnaryExpression", y)] =
      emptyType $ BinaryOperation Multiply (multiplicativeExpressionTransformer x) (unaryExpressionTransformer y)
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", x), ("/", _), ("UnaryExpression", y)] =
      emptyType $ BinaryOperation Divide (multiplicativeExpressionTransformer x) (unaryExpressionTransformer y)
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", x), ("%", _), ("UnaryExpression", y)] =
      emptyType $ BinaryOperation Modulus (multiplicativeExpressionTransformer x) (unaryExpressionTransformer y)

additiveExpressionTransformer :: TaggedParseTree -> Expression
additiveExpressionTransformer = match . asRule
  where
    match [("AdditiveExpression", _), ("MultiplicativeExpression", x)] =
      multiplicativeExpressionTransformer x
    match [("AdditiveExpression", _), ("AdditiveExpression", x), ("+", _), ("MultiplicativeExpression", y)] =
      emptyType $ BinaryOperation Add (additiveExpressionTransformer x) (multiplicativeExpressionTransformer y)
    match [("AdditiveExpression", _), ("AdditiveExpression", x), ("-", _), ("MultiplicativeExpression", y)] =
      emptyType $ BinaryOperation Subtract (additiveExpressionTransformer x) (multiplicativeExpressionTransformer y)

relationalExpressionTransformer :: TaggedParseTree -> Expression
relationalExpressionTransformer = match . asRule
  where
    match [("RelationalExpression", _), ("AdditiveExpression", x)] =
      additiveExpressionTransformer x
    match [("RelationalExpression", _), ("RelationalExpression", x), ("<", _), ("AdditiveExpression", y)] =
      emptyType $ BinaryOperation Less (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), (">", _), ("AdditiveExpression", y)] =
      emptyType $ BinaryOperation Greater (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), ("<=", _), ("AdditiveExpression", y)] =
      emptyType $ BinaryOperation LessEqual (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), (">=", _), ("AdditiveExpression", y)] =
      emptyType $ BinaryOperation GreaterEqual (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), ("instanceof", _), ("ReferenceType", y)] =
      emptyType $ Literal Void "TODO" --TODO
      -- emptyType $ BinaryOperation InstanceOf (relationalExpressionTransformer x) (referenceTypeTransformer y)

equalityExpressionTransformer :: TaggedParseTree -> Expression
equalityExpressionTransformer = match . asRule
  where
    match [("EqualityExpression", _), ("RelationalExpression", x)] =
      relationalExpressionTransformer x
    match [("EqualityExpression", _), ("EqualityExpression", x), ("==", _), ("RelationalExpression", y)] =
      emptyType $ BinaryOperation Equality (equalityExpressionTransformer x) (relationalExpressionTransformer y)
    match [("EqualityExpression", _), ("EqualityExpression", x), ("!=", _), ("RelationalExpression", y)] =
      emptyType $ BinaryOperation Inequality (equalityExpressionTransformer x) (relationalExpressionTransformer y)

andExpressionTransformer :: TaggedParseTree -> Expression
andExpressionTransformer = match . asRule
  where
    match [("AndExpression", _), ("EqualityExpression", x)] =
      equalityExpressionTransformer x
    match [("AndExpression", _), ("AndExpression", x), ("&", _), ("EqualityExpression", y)] =
      emptyType $ BinaryOperation LazyAnd (andExpressionTransformer x) (equalityExpressionTransformer y)

inclusiveOrExpressionTransformer :: TaggedParseTree -> Expression
inclusiveOrExpressionTransformer = match . asRule
  where
    match [("InclusiveOrExpression", _), ("AndExpression", x)] =
      andExpressionTransformer x
    match [("InclusiveOrExpression", _), ("InclusiveOrExpression", x), ("|", _), ("AndExpression", y)] =
      emptyType $ BinaryOperation LazyOr (inclusiveOrExpressionTransformer x) (andExpressionTransformer y)

conditionalAndExpressionTransformer :: TaggedParseTree -> Expression
conditionalAndExpressionTransformer = match . asRule
  where
    match [("ConditionalAndExpression", _), ("InclusiveOrExpression", x)] =
      inclusiveOrExpressionTransformer x
    match [("ConditionalAndExpression", _), ("ConditionalAndExpression", x), ("&&", _), ("InclusiveOrExpression", y)] =
      emptyType $ BinaryOperation And (conditionalAndExpressionTransformer x) (inclusiveOrExpressionTransformer y)

conditionalOrExpressionTransformer :: TaggedParseTree -> Expression
conditionalOrExpressionTransformer = match . asRule
  where
    match [("ConditionalOrExpression", _), ("ConditionalAndExpression", x)] =
      conditionalAndExpressionTransformer x
    match [("ConditionalOrExpression", _), ("ConditionalOrExpression", x), ("||", _), ("ConditionalAndExpression", y)] =
      emptyType $ BinaryOperation Or (conditionalOrExpressionTransformer x) (conditionalAndExpressionTransformer y)

assignmentExpressionTransformer :: TaggedParseTree -> Expression
assignmentExpressionTransformer = match . asRule
  where
    match [("AssignmentExpression", _), ("Assignment", _)] =
      emptyType $ Literal Void "TODO" --TODO
    match [("AssignmentExpression", _), ("ConditionalOrExpression", x)] =
      conditionalOrExpressionTransformer x

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
    match [("Expression", _), ("AssignmentExpression", x)] =
      assignmentExpressionTransformer x
