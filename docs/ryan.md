
# Code Generation with a Haskell Monad

Haskell is a purely functional language meaning functions are not allowed to
have any side-effects. All the consequence of calling a function is capture by
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

$\includegraphics{my_ps_file.ps}$


