Assignment 4
============

### Note:
* If all the steps are carried out as described, with no indication of abrupt
  completion, the statement is said to `complete normally`. However, certain
  events may prevent a statement from completing normally:
		* The return statements causes a transfer of control that may prevent
          normal completion of statements that contain them.

* If such an event occurs, then execution of one or more statements may be
  terminated before all steps of their normal mode of execution have completed,
  such statements are said to `complete abruptly`.
		1. A return with no value
        2. A return with a given value

* ConstantExpression(JLS 15.28):



### Reachability-check(JLS 14.20):
* The block that is the body of a constructor, method, instance initializer or
  static initializer is reachable;

* EB = empty block
		* EB can complete normally, iff it is reachable.
		* Non-eb can complete normally, iff the last stmt in it can complete
          normally.
		* The 1st stmt in a non-bm is reachable, iff the block is reachable.
		* All stmt S(except the first) is reachable, iff the statement preceding
          S can completely normally.

* A local class declaration statement can complete normally iff it is reachable

* A local variable declaration statement can complete normally iff it is
  reachable

* An empty stmt can complete normally, iff it is reachable. An expression
  statement can complete normally iff it is reachable

* A while stmt and contained stmts are reachable, iff `while` statement is
  reachable and condition expression is not constant expression with value True
  or False.

* A for stmt and contained stmts are reachable, iff `for` statement is reachable
  and condition expression is not constant expression with value True or False.

* An if-then stmt: the then-statement is reachable iff the if-then is reachable.

* An if-then-else stmt: The then-statement or else-statement is reachable iff
  the if-then-else stmt is reachable.

### what we need for RC now:
* If predicate must be a boolean: but the predicate can be initilized in
  constructor and method declaration
     * J1_7_Reachability_IfThenElse_InConstructor.java... FAILED
     * J1_7_Reachability_IfThenElse_InValueMethod.java... FAILED
     * J1_7_Reachability_IfThenElse_InVoidMethod.java... FAILED

* Method may only be invoked on reference types
     * test/marmoset/a4/positive/J1_7_Unreachable_IfEqualsNot.java... FAILED
     * test/marmoset/a4/positive/J1_arbitraryreturn.java... FAILED
     * test/marmoset/a4/positive/J1_omittedvoidreturn.java... FAILED
     * test/marmoset/a4/positive/J1_reachability_return... FAILED

* Local statement type doesn't match
     * test/marmoset/a4/positive/J1_unreachableAutomation.java... FAILED ->TODO
       not sure yet
     * test/marmoset/a4/positive/J1_Unreachable.java... FAILED -> check the type
       of variables in condition expression

* Negative tests
#### nothing after return
     * test/marmoset/a4/negative/Je_7_Reachability_AfterIfReturn.java... FAILED
       => no stmt can be reachable after return in if block
     * test/marmoset/a4/negative/Je_7_Reachability_AfterElseReturn.java...
       FAILED => no stmt can be reachable after return in else block
     * test/marmoset/a4/negative/Je_7_Reachability_AfterIfReturnElseReturn.java...
       FAILED => if `if and else` both have return stmt, then no stmts after
       return in the main block can be reachable
     * test/marmoset/a4/negative/Je_7_Reachability_AfterReturn_Constructor.java...
       FAILED => no constructor after return can be reachable
     * test/marmoset/a4/negative/Je_7_Reachability_AfterReturnEmptyBlock.java...
       FAILED => no empty block after return can be reachable
     * test/marmoset/a4/negative/Je_7_Reachability_AfterValueReturn.java...
       FAILED => no stmt after return can be reachable
     * test/marmoset/a4/negative/Je_7_Reachability_AfterVoidReturn.java...
       FAILED => no stmt after return can be reachable
     * test/marmoset/a4/negative/Je_7_Reachability_ReturnReturn.java... FAILED
       => no stmt after return, no return followed by return can be reachable


     * test/marmoset/a4/negative/Je_7_Reachability_EmptyValueMethod.java...
       FAILED => POINT 2, last stmt in a non-void-return-type method must be a
       return stmt.
     * test/marmoset/a4/negative/Je_7_Reachability_ForFalse_1.java... FAILED =>
       condition stmt in for should not be constant expression with value of
       True/False
     * test/marmoset/a4/negative/Je_7_Reachability_ForFalse_2.java... FAILED =>
       condition stmt in for should not be constant expression with value of
       True/False
     * test/marmoset/a4/negative/Je_7_Reachability_WhileFalse_ConstantFolding.java...
       FAILED=> condition stmt in while should't be constant expression with
       value of True/False
     * test/marmoset/a4/negative/Je_7_Reachability_WhileFalse_Empty.java...
       FAILED => condition stmt in while should't be constant expression with
       value of True/False
     * test/marmoset/a4/negative/Je_7_Reachability_WhileTrue_ConstantFolding.java...
       FAILED => condition stmt in while should't be constant expression with
       value of True/False
     * test/marmoset/a4/negative/Je_7_Reachability_WhileTrue.java... FAILED =>
       condition stmt in while should't be constant expression with value of
       True/False
     * test/marmoset/a4/negative/Je_7_Return_IfElseIf.java... FAILED => TODO:
       not sure why how to judge this test
     * test/marmoset/a4/negative/Je_7_Return_MissingInElse.java... FAILED =>
       missing return in else/method. TODO: if `if else` both have return in it,
       no return outside the if?
     * test/marmoset/a4/negative/Je_8_DefiniteAssignment_ComplexInitializer.java...
       FAILED => POINT 3, the variable must not occor in its own initilizer
     * test/marmoset/a4/negative/Je_8_DefiniteAssignment_FieldWithSameName.java...
       FAILED => POINT 3, each local variable must have an innitializer
     * test/marmoset/a4/negative/Je_8_DefiniteAssignment_InitToItself.java...
       FAILED => POINT 3, the variable must not occor in its own initilizer

* An error: 12: test/marmoset/a4/positive/J1_ifThenElse.java... ERROR

compiler:
src/lib/JoosCompiler/Ast/Transformers/StatementAndExpressionTransformers.hs:(140,5)-(149,31):
Non-exhaustive patterns in function match


### The last statement on every finite-length execution path through a method whose return type is not void must be a return statement.
 * Thought: Go through the AST, for each AstMethod node, check the method
   declaration,
    1. if it is not void, get the last children, check whether it is return
       statement. if it is not, then return compile-time error.
    2. if it is void, check there is no return statement in its chilren, if
       there is, return compile-time error


### Initialization:
Every local variable must have an initializer, and the variable must not occur
in its own initializer.
* Each local variable must have an initializer. It is solved by AST, if it
  doesn't do it, this local variable cause an error: parse error on token ;
  Expected one of : =
* The variable must not occur in its own initializer. For all localstatement,
  recursively check whether its children have the same Type as localStatement
  has.
