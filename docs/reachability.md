Assignment 4 
### Note:
* If all the steps are carried out as described, with no indication of abrupt completion, the statement is said to `complete normally`. However, certain events may prevent a statement from completing normally: 
		*The return statements causes a transfer of control that may prevent normal completion of statements that contain them.
* If such an event occurs, then execution of one or more statements may be terminated before all steps of their normal mode of execution have completed, such statements are said to `complete abruptly***.	
		**A return with no value
        **A return with a given value




### Reachability-check:
* The block that is the body of a constructor, method, instance initializer or static initializer is reachable; 
* EB = empty block
		EB can complete normally, iff it is reachable.
		Non-eb can complete normally, iff the last stmt in it can complete normally.
		The 1st stmt in a non-bm is reachable, iff the block is reachable.
		All stmt S(except the first) is reachable, iff the statement preceding S can completely normally.
* A local class declaration statement can complete normally iff it is reachable
* A local variable declaration statement can complete normally iff it is reachable
* An empty stmt can complete normally, iff it is reachable
6u. An expression statement can complete normally iff it is reachable
* A while stmt can complete normally, iff the while stmt is reachable && the condition expression is not a constant expression with value true.
   The contained stmt is reachable, iff the while stmt is reachable and the condition expression is not a constant expression whose value is false
* A for stmt can complete normally, iff the for stmt is reachable, there is a condition expression, and the condition expression is not a constant expression with value true.
   The contained statement is reachable iff the for statement is reachable and the condtion expression is not a constant expression whose value is false.
* An if-then stmt can complete normally, iff it is reachable. The then-statement is reachable iff the if-then is reachable.
  An if-then-else stmt can complete normally, iff the then-stmt can complete normally or the else-stmt can complete normally. The then-statement is reachable iff the if-then-else stmt is reachable. The else-stmt is reachable iff the if-then-else stmt is reachable.

### The last statement on every finite-length execution path through a method whose return type is not void must be a return statement. 
		a method return type if not void
		finite-length execution
		the last stmt must be a return stmt

### Initialization:
Every local variable must have an initializer, and the variable must not occur in its own initializer. 














