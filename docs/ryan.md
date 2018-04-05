
# Dynamic Dispatch and the VTable

We did not allocate enough time to implement and test dynamic dispatch; however, we
discussed a possible implementation. This implementation is similar to the "Big
Ugly Table" (BUT) discussed in lecture. Generating the code has three steps:

1. Only non-abstract classes need vtables.
2.
3.


For example, consider the following class hierarchy and corresponding table.
Step 1 generates the first column, the union of all virtual, qualified methods.
Step 2 creates additional columns for each concrete class. Step 3

                                             |
        abstract class Object {              |
            abstract void f();               |
            abstract void g();               |
        };                                   |    abstract class C extends Object {
                                             |        abstract int h();
        abstract class A extends Object {    |        abstract int f();
            void f() {}                      |    }
            abstract void h() {}             |
        };                                   |    class D extends C {
                                             |        int f() {}
        class B extends A {                  |        int g() {}
            int g() {}                       |        int h() {}
            int h() {}                       |    }
        }                                    |
                                             |


        | Union    | B VTable | D VTable |
        |----------|----------|----------|
        | Object.f | A.f      | D.f      |
        | Object.g | B.g      | D.g      |
        | A.h      | B.h      | NULL     |
        | C.h      | NULL     | D.h      |
        | C.f      | NULL     | D.f      |


We did not have the time to implement this; however, we found static dispatch a
suitable replacement in many (but not all) situations.


# Code Generation with a Haskell Monad

Haskell is a purely functional language meaning functions are not allowed to
have any side-effects. All the consequence of calling a function is captured in
its return value.

This proved difficult when side-effects were needed to generate globally unique
labels during code generation. A conventional programming language would
generate unique labels with an idiom like the following:

    int i = 0;
    int nextCounter() {
      return i++;
    }

However, this construct is not possible in Haskell because it modifies global
state which is a side-effect. To circumvent this, we used a `Monad` which is
able to combines with data

For example, the following listing is a Haskell function generating code for
the logical AND operator (&&) template. Calling this function generates code
for itself and its sub-expressions.

    generateExpression ctx (BinaryOperation And x y) = do
      generateExpression' ctx x     -- Generate sub-expression
      cmp Eax (I 1)                 -- Compare Eax to the integer 1
      l <- uniqueLabel              -- Generate a new unique label
      jne (L l)                     --
      generateExpression' ctx y     --
      label l                       --

The original expression `false && true`, the following AST is passed into
`generateExpression`:

    (BinaryExpression And
      (BooleanLiteral False)
      (BooleanLiteral True))

The generated code looks something the following expression:

    ; BinaryOperation(LiteralExpression(true) && LiteralExpression(false))
      ; LiteralExpression(true)
      mov eax, 1;
    cmp eax, 1;
    jne label6;
      ; LiteralExpression(false)
      mov eax, 0;
    label6:

Do-notation allows state to be hidden behind the scenes.

# Profile

![profile](profile.png)


