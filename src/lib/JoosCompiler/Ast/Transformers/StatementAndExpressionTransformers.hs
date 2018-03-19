module JoosCompiler.Ast.Transformers.StatementAndExpressionTransformers
  ( typeTransformer
  , methodBodyTransformer
  , constructorBodyTransformer
  , expressionTransformer
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

literalTransformer :: TaggedParseTree -> Literal
literalTransformer = match . asRule
  where
    match [("Literal", _), ("IntegerLiteral", x)] =
      IntegerLiteral (read $ tokenString $ lhs x)
    match [("Literal", _), ("BooleanLiteral", x)] =
      BooleanLiteral (if tokenString (lhs x) == "true" then True else False)
    match [("Literal", _), ("CharacterLiteral", x)] =
      CharacterLiteral (head $ tokenString $ lhs x) -- TODO: escapes
    match [("Literal", _), ("StringLiteral", x)] =
      StringLiteral (tokenString $ lhs x) -- TODO: escapes
    match [("Literal", _), ("NullLiteral", x)] =
      NullLiteral

typeTransformer :: TaggedParseTree -> Type
typeTransformer = match . asRule
  where
    match [("Type", _), ("PrimitiveType", x)] =
      Type (primitiveTypeTransformer x) False
    match [("Type", _), ("ReferenceType", x)] =
      referenceTypeTransformer x

primitiveTypeTransformer :: TaggedParseTree -> InnerType
primitiveTypeTransformer = match . asRule
  where
    match [("PrimitiveType", _), ("boolean", x)] =
      Boolean
    match [("PrimitiveType", _), ("byte", x)] =
      Byte
    match [("PrimitiveType", _), ("char", x)] =
      Char
    match [("PrimitiveType", _), ("int", x)] =
      Int
    match [("PrimitiveType", _), ("short", x)] =
      Short

referenceTypeTransformer :: TaggedParseTree -> Type
referenceTypeTransformer = match . asRule
  where
    match [("ReferenceType", _), ("Name", x)] =
      Type (NamedType $ nameTransformer x) False
    match [("ReferenceType", _), ("ArrayType", x)] =
      arrayTypeTransformer x

arrayTypeTransformer :: TaggedParseTree -> Type
arrayTypeTransformer = match . asRule
  where
    match [("ArrayType", _), ("PrimitiveType", x), ("[", _), ("]", _)] =
      Type (primitiveTypeTransformer x) False
    match [("ArrayType", _), ("Name", x), ("[", _), ("]", _)] =
      Type (NamedType $ nameTransformer x) True

nameTransformer :: TaggedParseTree -> Name
nameTransformer = match . asRule
  where
    match [("Name", _), ("Identifier", x)] =
      [tokenString $ lhs x]
    match [("Name", _), ("Name", xs), (".", _), ("Identifier", x)] =
      nameTransformer xs ++ [tokenString $ lhs x]

methodBodyTransformer :: TaggedParseTree -> Statement
methodBodyTransformer = match . asRule
  where
    match [("MethodBody", _), ("Block", x)] =
      if statement == TerminalStatement then EmptyStatement TerminalStatement else statement
      where statement = blockTransformer x
    match [("MethodBody", _), (";", _)] =
      TerminalStatement

blockTransformer :: TaggedParseTree -> Statement
blockTransformer = match . asRule
  where
    match [("Block", _), ("{", _), ("}", _)] =
      TerminalStatement
    match [("Block", _), ("{", _), ("BlockStatements", x), ("}", _)] =
      foldr (\x y -> x{nextStatement=y}) TerminalStatement (blockStatementsTransformer x)

constructorBodyTransformer :: TaggedParseTree -> Statement
constructorBodyTransformer = match . asRule
  where
    match [("ConstructorBody", _), ("{", _), ("}", _)] =
      EmptyStatement TerminalStatement
    match [("ConstructorBody", _), ("{", _), ("BlockStatements", x), ("}", _)] =
      foldr (\x y -> x{nextStatement=y}) TerminalStatement (blockStatementsTransformer x)

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
      localVariableDeclarationStatementTransformer x
    match [("BlockStatement", _), ("Statement", x)] =
      statementTransformer x

localVariableDeclarationStatementTransformer :: TaggedParseTree -> Statement
localVariableDeclarationStatementTransformer = match . asRule
  where
    match [("LocalVariableDeclarationStatement", _), ("LocalVariableDeclaration", x), (";", _)] =
      localVariableDeclarationTransformer x

localVariableDeclarationTransformer :: TaggedParseTree -> Statement
localVariableDeclarationTransformer = match . asRule
  where
    match [("LocalVariableDeclaration", _), ("Type", t), ("Identifier", n), ("=", _), ("Expression", x)] =
      LocalStatement
        { localVariable = Variable
          { variableType      = (typeTransformer t)
          , variableModifiers = []
          , variableName      = tokenString $ lhs n
          , variableValue     = expressionTransformer x
          }
        , nextStatement = TerminalStatement }

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
    match [(i, _), (j, _)] =
      error $ "statementTransformer saw: " ++ commaDelimit [i, j]
    match [(i, _)] =
      error $ "statementTransformer got one-member list: " ++ i
    match [] =
      error "statementTransformer got empty list"

statementWithoutTrailingSubstatementTransformer :: TaggedParseTree -> Statement
statementWithoutTrailingSubstatementTransformer = match . asRule
  where
    match [("StatementWithoutTrailingSubstatement", _), ("Block", x)] =
      BlockStatement
        { statementBlock = blockTransformer x
        , nextStatement = TerminalStatement }
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
      EmptyStatement
        { nextStatement = TerminalStatement }

expressionStatementTransformer :: TaggedParseTree -> Statement
expressionStatementTransformer = match . asRule
  where
    match [("ExpressionStatement", _), ("StatementExpression", x), (";", _)] =
      statementExpressionTransformer x

statementExpressionTransformer :: TaggedParseTree -> Statement
statementExpressionTransformer = match . asRule
  where
    match [("StatementExpression", _), ("Assignment", x)] =
      ExpressionStatement
        { statementExpression = assignmentTransformer x
        , nextStatement = TerminalStatement }
    match [("StatementExpression", _), ("MethodInvocation", x)] =
      ExpressionStatement
        { statementExpression = methodInvocationTransformer x
        , nextStatement = TerminalStatement }
    match [("StatementExpression", _), ("ClassInstanceCreationExpression", x)] =
      ExpressionStatement
        { statementExpression = classInstanceCreationExpressionTransformer x
        , nextStatement = TerminalStatement }

ifThenStatementTransformer :: TaggedParseTree -> Statement
ifThenStatementTransformer = match . asRule
  where
    match [("IfThenStatement", _), ("if", _), ("(", _), ("Expression", e), (")", _), ("Statement", s)] =
      IfStatement
        { ifPredicate = expressionTransformer e
        , ifThenStatement = statementTransformer s
        , ifElseStatement = TerminalStatement
        , nextStatement = TerminalStatement }

ifThenElseStatementTransformer :: TaggedParseTree -> Statement
ifThenElseStatementTransformer = match . asRule
  where
    match [("IfThenElseStatement", _), ("if", _), ("(", _), ("Expression", e), (")", _), ("StatementNoShortIf", s1), ("else", _), ("Statement", s2)] =
      IfStatement
        { ifPredicate = expressionTransformer e
        , ifThenStatement = statementNoShortIfTransformer s1
        , ifElseStatement = statementTransformer s2
        , nextStatement = TerminalStatement }

ifThenElseStatementNoShortIfTransformer :: TaggedParseTree -> Statement
ifThenElseStatementNoShortIfTransformer = match . asRule
  where
    match [("IfThenElseStatementNoShortIf", _), ("if", _), ("(", _), ("Expression", e), (")", _), ("StatementNoShortIf", s1), ("else", _), ("StatementNoShortIf2", s2)] =
      IfStatement
        { ifPredicate = expressionTransformer e
        , ifThenStatement = statementNoShortIfTransformer s1
        , ifElseStatement = statementNoShortIfTransformer2 s2
        , nextStatement = TerminalStatement }

statementNoShortIfTransformer2 :: TaggedParseTree -> Statement
statementNoShortIfTransformer2 = match . asRule
  where
    match [("StatementNoShortIf2", _), ("StatementNoShortIf", s)] =
      statementNoShortIfTransformer s

whileStatementTransformer :: TaggedParseTree -> Statement
whileStatementTransformer = match . asRule
  where
    match [("WhileStatement", _), ("while", _), ("(", _), ("Expression", e), (")", _), ("Statement", s)] =
      LoopStatement
        { loopPredicate = expressionTransformer e
        , loopStatement = statementTransformer s
        , nextStatement = TerminalStatement }

whileStatementNoShortIfTransformer :: TaggedParseTree -> Statement
whileStatementNoShortIfTransformer = match . asRule
  where
    match [("WhileStatementNoShortIf", _), ("while", _), ("(", _), ("Expression", e), (")", _), ("StatementNoShortIf", s)] =
      LoopStatement
        { loopPredicate = expressionTransformer e
        , loopStatement = statementNoShortIfTransformer s
        , nextStatement = TerminalStatement }

forStatementTransformer :: TaggedParseTree -> Statement
forStatementTransformer = match . asRule
  where
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("Statement", x)] =
      genericForLoop TerminalStatement (LiteralExpression $ StringLiteral "true") TerminalStatement (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (LiteralExpression $ StringLiteral "true") TerminalStatement (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), (")", _), ("Statement", x)] =
      genericForLoop TerminalStatement (expressionTransformer e) TerminalStatement (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) TerminalStatement (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop TerminalStatement (LiteralExpression $ StringLiteral "true") (forUpdateTransformer s2) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (LiteralExpression $ StringLiteral "true") (forUpdateTransformer s2) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop TerminalStatement (expressionTransformer e) (forUpdateTransformer s2) (statementTransformer x)
    match [("ForStatement", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("Statement", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) (forUpdateTransformer s2) (statementTransformer x)

forStatementNoShortIfTransformer :: TaggedParseTree -> Statement
forStatementNoShortIfTransformer = match . asRule
  where
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop TerminalStatement (LiteralExpression $ StringLiteral "true") TerminalStatement (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (LiteralExpression $ StringLiteral "true") TerminalStatement (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop TerminalStatement (expressionTransformer e) TerminalStatement (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) TerminalStatement (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop TerminalStatement (LiteralExpression $ StringLiteral "true") (forUpdateTransformer s2) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (LiteralExpression $ StringLiteral "true") (forUpdateTransformer s2) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop TerminalStatement (expressionTransformer e) (forUpdateTransformer s2) (statementNoShortIfTransformer x)
    match [("ForStatementNoShortIf", _), ("for", _), ("(", _), ("ForInit", s1), (";", _), ("Expression", e), (";", _), ("ForUpdate", s2), (")", _), ("StatementNoShortIf", x)] =
      genericForLoop (forInitTransformer s1) (expressionTransformer e) (forUpdateTransformer s2) (statementNoShortIfTransformer x)

-- Helper function for for-statements
genericForLoop init expr update statement =
  BlockStatement
    { nextStatement = TerminalStatement,
      statementBlock =
        init{ nextStatement =
          LoopStatement
          { nextStatement = TerminalStatement
          , loopPredicate = expr
          , loopStatement =
            BlockStatement
            { statementBlock = statement
            , nextStatement = update } } } }

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
      ReturnStatement
        { returnExpression = Nothing
        , nextStatement = TerminalStatement }
    match [("ReturnStatement", _), ("return", _), ("Expression", x), (";", _)] =
      ReturnStatement
        { returnExpression = Just $ expressionTransformer x
        , nextStatement = TerminalStatement }

primaryTransformer :: TaggedParseTree -> Expression
primaryTransformer = match . asRule
  where
    match [("Primary", _), ("PrimaryNoNewArray", x)] =
      primaryNoNewArrayTransformer x
    match [("Primary", _), ("ArrayCreationExpression", x)] =
      arrayCreationExpressionTransformer x

primaryNoNewArrayTransformer :: TaggedParseTree -> Expression
primaryNoNewArrayTransformer = match . asRule
  where
    match [("PrimaryNoNewArray", _), ("Literal", x)] =
      LiteralExpression $ literalTransformer x
    match [("PrimaryNoNewArray", _), ("this", _)] =
      This
    match [("PrimaryNoNewArray", _), ("(", _), ("Expression", x), (")", _)] =
      expressionTransformer x
    match [("PrimaryNoNewArray", _), ("ClassInstanceCreationExpression", x)] =
      classInstanceCreationExpressionTransformer x
    match [("PrimaryNoNewArray", _), ("FieldAccess", x)] =
      fieldAccessTransformer x
    match [("PrimaryNoNewArray", _), ("MethodInvocation", x)] =
      methodInvocationTransformer x
    match [("PrimaryNoNewArray", _), ("ArrayAccess", x)] =
      arrayAccessTransformer x

classInstanceCreationExpressionTransformer :: TaggedParseTree -> Expression
classInstanceCreationExpressionTransformer = match . asRule
  where
    match [("ClassInstanceCreationExpression", _), ("new", _), ("Name", x), ("(", _), (")", _)] =
      NewExpression (nameTransformer x) []
    match [("ClassInstanceCreationExpression", _), ("new", _), ("Name", x), ("(", _), ("ArgumentList", y), (")", _)] =
      NewExpression (nameTransformer x) (argumentListTransformer y)

argumentListTransformer :: TaggedParseTree -> [Expression]
argumentListTransformer = match . asRule
  where
    match [("ArgumentList", _), ("Expression", x)] =
      [expressionTransformer x]
    match [("ArgumentList", _), ("ArgumentList", xs), (",", _), ("Expression", x)] =
      argumentListTransformer xs ++ [expressionTransformer x]

arrayCreationExpressionTransformer :: TaggedParseTree -> Expression
arrayCreationExpressionTransformer = match . asRule
  where
    match [("ArrayCreationExpression", _), ("new", _), ("Name", t), ("[", _), ("Expression", e), ("]", _)] =
      NewArrayExpression (Type (NamedType $ nameTransformer t) False) (expressionTransformer e) -- TODO: convert char to Char, etc..
    match [("ArrayCreationExpression", _), ("new", _), ("PrimitiveType", t), ("[", _), ("Expression", e), ("]", _)] =
      NewArrayExpression (Type (primitiveTypeTransformer t) False) (expressionTransformer e)

fieldAccessTransformer :: TaggedParseTree -> Expression
fieldAccessTransformer = match . asRule
  where
    match [("FieldAccess", _), ("Primary", x), (".", _), ("Identifier", y)] =
      FieldAccess (primaryTransformer x) (tokenString $ lhs y)

methodInvocationTransformer :: TaggedParseTree -> Expression
methodInvocationTransformer = match . asRule
  where
    match [("MethodInvocation", _), ("Name", n), ("(", _), (")", _)] =
      MethodInvocation expression (last name) []
      where name = nameTransformer n
            expression = if null (init name) then This else ExpressionName (init name)
    match [("MethodInvocation", _), ("Name", n), ("(", _), ("ArgumentList", xs), (")", _)] =
      MethodInvocation expression (last name) (argumentListTransformer xs)
      where name = nameTransformer n
            expression = if null (init name) then This else ExpressionName (init name)
    match [("MethodInvocation", _), ("Primary", e), (".", _), ("Identifier", n), ("(", _), (")", _)] =
      MethodInvocation (primaryTransformer e) (tokenString $ lhs n) []
    match [("MethodInvocation", _), ("Primary", e), (".", _), ("Identifier", n), ("(", _), ("ArgumentList", xs), (")", _)] =
      MethodInvocation (primaryTransformer e) (tokenString $ lhs n) (argumentListTransformer xs)

arrayAccessTransformer :: TaggedParseTree -> Expression
arrayAccessTransformer = match . asRule
  where
    match [("ArrayAccess", _), ("Name", e1), ("[", _), ("Expression", e2), ("]", _)] =
      ArrayExpression (ExpressionName $ nameTransformer e1) (expressionTransformer e2)
    match [("ArrayAccess", _), ("PrimaryNoNewArray", e1), ("[", _), ("Expression", e2), ("]", _)] =
      ArrayExpression (primaryNoNewArrayTransformer e1) (expressionTransformer e2)

unaryExpressionTransformer :: TaggedParseTree -> Expression
unaryExpressionTransformer = match . asRule
  where
    match [("UnaryExpression", _), ("-", _), ("UnaryExpression", x)] =
      UnaryOperation Negate (unaryExpressionTransformer x)
    match [("UnaryExpression", _), ("UnaryExpressionNotPlusMinus", x)] =
      unaryExpressionNotPlusMinusTransformer x

unaryExpressionNotPlusMinusTransformer :: TaggedParseTree -> Expression
unaryExpressionNotPlusMinusTransformer = match . asRule
  where
    match [("UnaryExpressionNotPlusMinus", _), ("Primary", x)] =
      primaryTransformer x
    match [("UnaryExpressionNotPlusMinus", _), ("Name", x)] =
      ExpressionName (nameTransformer x)
    match [("UnaryExpressionNotPlusMinus", _), ("!", _), ("UnaryExpression", x)] =
      UnaryOperation Not (unaryExpressionTransformer x)
    match [("UnaryExpressionNotPlusMinus", _), ("CastExpression", x)] =
      castExpressionTransformer x

castExpressionTransformer :: TaggedParseTree -> Expression
castExpressionTransformer = match . asRule
  where
    match [("CastExpression", _), ("(", _), ("PrimitiveType", t), (")", _), ("UnaryExpression", x)] =
      CastExpression (Type (primitiveTypeTransformer t) False) (unaryExpressionTransformer x)
    match [("CastExpression", _), ("(", _), ("PrimitiveType", t), ("[", _), ("]", _), (")", _), ("UnaryExpression", x)] =
      CastExpression (Type (primitiveTypeTransformer t) False) (unaryExpressionTransformer x)
    match [("CastExpression", _), ("(", _), ("Expression", t), (")", _), ("UnaryExpressionNotPlusMinus", x)] =
      CastExpression (getType $ expressionTransformer t) (unaryExpressionNotPlusMinusTransformer x)
      where getType (ExpressionName name) = Type (NamedType name) False -- TODO: this doesn't seem right
            getType _                     = error "Cast expression name should have been checked by the weeder"
    match [("CastExpression", _), ("(", _), ("Name", t), ("[", _), ("]", _), (")", _), ("UnaryExpressionNotPlusMinus", x)] =
      CastExpression (Type (NamedType $ nameTransformer t) False) (unaryExpressionNotPlusMinusTransformer x)

multiplicativeExpressionTransformer :: TaggedParseTree -> Expression
multiplicativeExpressionTransformer = match . asRule
  where
    match [("MultiplicativeExpression", _), ("UnaryExpression", x)] =
      unaryExpressionTransformer x
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", x), ("*", _), ("UnaryExpression", y)] =
      BinaryOperation Multiply (multiplicativeExpressionTransformer x) (unaryExpressionTransformer y)
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", x), ("/", _), ("UnaryExpression", y)] =
      BinaryOperation Divide (multiplicativeExpressionTransformer x) (unaryExpressionTransformer y)
    match [("MultiplicativeExpression", _), ("MultiplicativeExpression", x), ("%", _), ("UnaryExpression", y)] =
      BinaryOperation Modulus (multiplicativeExpressionTransformer x) (unaryExpressionTransformer y)

additiveExpressionTransformer :: TaggedParseTree -> Expression
additiveExpressionTransformer = match . asRule
  where
    match [("AdditiveExpression", _), ("MultiplicativeExpression", x)] =
      multiplicativeExpressionTransformer x
    match [("AdditiveExpression", _), ("AdditiveExpression", x), ("+", _), ("MultiplicativeExpression", y)] =
      BinaryOperation Add (additiveExpressionTransformer x) (multiplicativeExpressionTransformer y)
    match [("AdditiveExpression", _), ("AdditiveExpression", x), ("-", _), ("MultiplicativeExpression", y)] =
      BinaryOperation Subtract (additiveExpressionTransformer x) (multiplicativeExpressionTransformer y)

relationalExpressionTransformer :: TaggedParseTree -> Expression
relationalExpressionTransformer = match . asRule
  where
    match [("RelationalExpression", _), ("AdditiveExpression", x)] =
      additiveExpressionTransformer x
    match [("RelationalExpression", _), ("RelationalExpression", x), ("<", _), ("AdditiveExpression", y)] =
      BinaryOperation Less (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), (">", _), ("AdditiveExpression", y)] =
      BinaryOperation Greater (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), ("<=", _), ("AdditiveExpression", y)] =
      BinaryOperation LessEqual (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), (">=", _), ("AdditiveExpression", y)] =
      BinaryOperation GreaterEqual (relationalExpressionTransformer x) (additiveExpressionTransformer y)
    match [("RelationalExpression", _), ("RelationalExpression", x), ("instanceof", _), ("ReferenceType", y)] =
      InstanceOfExpression (relationalExpressionTransformer x) (referenceTypeTransformer y)

equalityExpressionTransformer :: TaggedParseTree -> Expression
equalityExpressionTransformer = match . asRule
  where
    match [("EqualityExpression", _), ("RelationalExpression", x)] =
      relationalExpressionTransformer x
    match [("EqualityExpression", _), ("EqualityExpression", x), ("==", _), ("RelationalExpression", y)] =
      BinaryOperation Equality (equalityExpressionTransformer x) (relationalExpressionTransformer y)
    match [("EqualityExpression", _), ("EqualityExpression", x), ("!=", _), ("RelationalExpression", y)] =
      BinaryOperation Inequality (equalityExpressionTransformer x) (relationalExpressionTransformer y)

andExpressionTransformer :: TaggedParseTree -> Expression
andExpressionTransformer = match . asRule
  where
    match [("AndExpression", _), ("EqualityExpression", x)] =
      equalityExpressionTransformer x
    match [("AndExpression", _), ("AndExpression", x), ("&", _), ("EqualityExpression", y)] =
      BinaryOperation LazyAnd (andExpressionTransformer x) (equalityExpressionTransformer y)

inclusiveOrExpressionTransformer :: TaggedParseTree -> Expression
inclusiveOrExpressionTransformer = match . asRule
  where
    match [("InclusiveOrExpression", _), ("AndExpression", x)] =
      andExpressionTransformer x
    match [("InclusiveOrExpression", _), ("InclusiveOrExpression", x), ("|", _), ("AndExpression", y)] =
      BinaryOperation LazyOr (inclusiveOrExpressionTransformer x) (andExpressionTransformer y)

conditionalAndExpressionTransformer :: TaggedParseTree -> Expression
conditionalAndExpressionTransformer = match . asRule
  where
    match [("ConditionalAndExpression", _), ("InclusiveOrExpression", x)] =
      inclusiveOrExpressionTransformer x
    match [("ConditionalAndExpression", _), ("ConditionalAndExpression", x), ("&&", _), ("InclusiveOrExpression", y)] =
      BinaryOperation And (conditionalAndExpressionTransformer x) (inclusiveOrExpressionTransformer y)

conditionalOrExpressionTransformer :: TaggedParseTree -> Expression
conditionalOrExpressionTransformer = match . asRule
  where
    match [("ConditionalOrExpression", _), ("ConditionalAndExpression", x)] =
      conditionalAndExpressionTransformer x
    match [("ConditionalOrExpression", _), ("ConditionalOrExpression", x), ("||", _), ("ConditionalAndExpression", y)] =
      BinaryOperation Or (conditionalOrExpressionTransformer x) (conditionalAndExpressionTransformer y)

assignmentExpressionTransformer :: TaggedParseTree -> Expression
assignmentExpressionTransformer = match . asRule
  where
    match [("AssignmentExpression", _), ("Assignment", x)] =
      assignmentTransformer x
    match [("AssignmentExpression", _), ("ConditionalOrExpression", x)] =
      conditionalOrExpressionTransformer x

assignmentTransformer :: TaggedParseTree -> Expression
assignmentTransformer = match . asRule
  where
    match [("Assignment", _), ("Name", x), ("=", _), ("AssignmentExpression", y)] =
      BinaryOperation Assign (ExpressionName (nameTransformer x)) (assignmentExpressionTransformer y)
    match [("Assignment", _), ("FieldAccess", x), ("=", _), ("AssignmentExpression", y)] =
      BinaryOperation Assign (fieldAccessTransformer x) (assignmentExpressionTransformer y)
    match [("Assignment", _), ("ArrayAccess", x), ("=", _), ("AssignmentExpression", y)] =
      BinaryOperation Assign (arrayAccessTransformer x) (assignmentExpressionTransformer y)

expressionTransformer :: TaggedParseTree -> Expression
expressionTransformer = match . asRule
  where
    match [("Expression", _), ("AssignmentExpression", x)] =
      assignmentExpressionTransformer x
