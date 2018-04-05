Summary of Phases
=================

As recommended in the course material, our compiler was broken down into the
following phases:

* Scanning
* Parsing
* Ast building
* Environment building and type linking
* Hierarchy checking
* Type checking and reachability checking
* Code generation

Phases before Code Generation
=============================

The compiler is split into 4 files that are invoked in order by joosc. Those
are:

* `bin/lexer`
* `bin/parser`
* `bin/weeder`
* `bin/compiler`

Everything starting with the AST building phases is done in `bin/compiler`. The
shared Haskell code is stored in `src/lib/JoosCompiler`.

Type linking and disambiguation were not done in time in our compiler. They were
only finished after Assignment 4. Included is a short description of how they
work since they were not fully described in our previous report.

Because we used Haskell for our compiler, we could not use pointers in type
linking. So type linking was in effect "type canonicalization", where we
converted all named types in the tree to their canonical names. This was done by
searching in the imported files for a name that matches the one we're
canonicalizing.

Disambiguation worked by replacing `ExpressionName` and `AmbiguousFieldAccess`
nodes with the proper type of access node (e.g. `DynamicFieldAccess`). One type
of access is `ClassAccess`, which is an intermediate that is turned into either
a `StaticMethodInvocation` or `StaticFieldAccess`.

For a detailed description of the phases that precede code generation, see
reports 1 and 2.

Testing
=======

Our primary method for testing the compiler was the Marmoset test suite. We have
downloaded all the Marmoset tests and stored them in the `test/marmoset`
directory. Each assignment's tests are in their own directory, and each of those
directories contains `positive` and `negative` tests. There are also tests in
`test/codegen` and `test/java-exploration`, which we used for initial code
generation testing and for testing what is legal Java and what isn't.

Our development process was to agree on a breakdown of tasks and work on them,
running `make test.a5` for each change to test the progress we made.

For testing the generated code, we found gdb to be very useful in debugging. We
started gdb by running `gdb --tui output/main` and used the following commands
for stepping through the generated program.

```
(gdb) b _start
(gdb) r
(gdb) n
(gdb) layout regs
```
