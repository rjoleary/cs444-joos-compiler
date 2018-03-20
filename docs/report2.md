# Joos Compiler Design

* Ahmed Al-Sudani (a2alsuda)
* Ryan O'Leary (rj2olear)
* Xia Liu (x397liu)

# Code Structure

## Directory Structure

* `src/lexer/`: Haskell source code for the lexer
* `src/rust/`: Rust source code for the parse engine
* `src/java/`: Java source code for the parse generator
* `src/weeder/`: Haskell source code for the weeder
* `src/compiler/`: Haskell source code for the rest of the compilation phases
* `def/`: Joos grammar and other definition files
* `def/joos.cfg2`: Joos production rules
* `test/positive/`: Test inputs which are expected to pass
* `test/negative/`: Test inputs which are expected to fail
* `test/haskell/`: Unit tests written in Haskell which currently only
  test the Lexer stage
* `test/marmoset/`: Marmoset tests used as test inputs
* `MAKEFILE`: Builds compiler, build documentation, runs tests, etc...
* `joosc`: A script which ties all the stages together
* `stdjoosc`: Invokes `joosc` on the stdlib as well as the input Java files

## Build System

In addition to building compiler, `MAKEFILE` contains a number of other useful
utilities:

    make                # Build the compiler
    make grammar        # Rebuild the parse table
    make test.a$n       # Run tests for assignment n
    make test.unit      # Run Haskell unit tests
    make test           # Run our own test files
    make zip            # Generate zip file for submitting to Marmoset
    make docs           # Compile the docs into PDFs
    make clean          # Delete intermediate files

# Phases in A2-A4

<!-- Don't forget to add a lot of technical details this time -->

## AST Generation

Code in: `src/lib/JoosCompiler/Ast`

We did not finish the AST generation phase in A1, so we did most of the AST work
in A2.

Our compiler gradually shifted from using the parse tree to an AST. This
was accomplished by keeping the parse tree structure and replacing the nodes in
the  tree by AST nodes one at a time. The conversion from parse tree to AST is
driven by `Ast/Core.hs`, which traverses the parse tree and calls the correct
transformer for each node if the transformer is defined.

The default transformer returns the original parse tree.

Because we used Haskell for our compiler, replacing the parse tree nodes was not
as simple as returning the AST node. It was necessary to create two levels of
new types:

  * The AST Node Types (`src/lib/JoosCompiler/Ast/NodeTypes.hs`)
  * The AST Wrapper Type (`src/lib/JoosCompiler/Ast/Transformers/Types.hs`)

The reason that was necessary is that the AST Node Types were disjoint by
design, and in order to combine them into a tree in a way that Haskell accepts,
we needed to create a "tagged union", which the AST Wrapper is. One benefit of
this approach is that it allowed us to also wrap parse tree nodes with the AST Wrapper.

By the time we were working on A4, the AST contained all the input information.
So the compiler discarded the parse tree nodes starting then since the data they
contained was redundant at that point.

One issue that we struggled with several times is the immutability and lack of
reference types in Haskell. Every time a transformation was added to the AST, we
had to decide where the data would be updated since it was stored in multiple
locations.

For example, type declarations were added to the compilation unit
nodes, but they were also children of that node. For that case, we decided to
update both. In other cases, we usually updated one node and treated it as the
source of truth, and the other nodes were discarded. We also tried to reduce the
number of places where this issue can occur by treating the data in one place
where possible and storing the name everywhere else, which could be treated as a
"pointer".

## AST Details

(This is documentation for our team, so it might go too deep!)

### How the AST is converted

There is a function `cstToAst` in
[Core.hs](src/lib/haskell/JoosCompiler/Ast/Core.hs). This function
recursively calls the correct "transformer" on each node of the tree.

A transformer is a function with signature
`[AstNode] -> TaggedParseTree -> AstWrapper`. The reason for that
weird signature is that it accepts those two arguments:

- transformedChildren, a list of the node's children after they had
  been transformed with `cstToAst`
- rootNode, the node that we are going to transform now

The reason behind this signature is that we want to have access to the ASTs
for the children nodes by the time we get to the root node. This makes
our work much easier since it's easier to work with the AST than the
CST, and the children are ASTs.

For example, we don't have to traverse the tree to find out the type of a
variable. We can simply look for its `AstType` node.

There is a bunch of transformers that we have defined in
[src/lib/JoosCompiler/Ast/Transformers](src/lib/haskell/JoosCompiler/Ast/Transformers).
The correct transformer is picked by `getTransformer` in
[Core.hs](src/lib/JoosCompiler/Ast/Core.hs), which decides
based on the `tokenName` (which can be `Identifier`, `Modifier`,
`int`, etc.). The complete list is in `def/joos.cfg2`, of course,
which defines our grammar.

### AST Types

The types for our AST are messy and a little convoluted. The reasons
are:

1. Haskell does not allow some type operations that would have made
   our types simpler
2. We are trying to take advantage of type checking as much as we can,
   even if our types become somewhat cumbersome

The types are essentially:

* `a`, where `a` stands for some node type defined in
  [src/lib/JoosCompiler/Ast/NodeTypes.hs](src/lib/haskell/JoosCompiler/Ast/NodeTypes.hs). This
  can be something like `Type`, `ClassDeclaration`, `Block`, etc.

* `AstWrapper`, which wraps the node types in `NodeTypes.hs`. It can
  also wrap a `TaggedToken`, which is how we keep the parse tree

* `AstNode :: Tree AstWrapper`

## Environment Building

In our compiler, we use `Scope`s to represent environments.

A scope belongs to a `Block`, and it stores:

  * Local declarations
  * The parent scope (or Nothing if no parent)
  * The Compilation Unit which the scope belongs to

Thus the scope makes it possible to resolve all names at any point inside a
method. That is done by checking for a local that matches the name. If no local
matches, the fields are checked (they are found using the compilation unit,
which points to the type declaration). If no field matches, then we check the
imported packages (also through the compilation unit)

The error checking for this phase is done in `src/compiler/NameResolution/EnvironmentBuilding.hs`

## Type Linking and Hierarchy Checking

<!--

- Dealing with multiple files
- How hierarchy is represented (packages, subpackages, types)
- Type canonicalization replaces all names (whether used as types in
  declarations or as compound names for fields)
- We have functions like resolveTypeInProgram, resolveInScope that allow us to
  look the declaration and get all the information we need

-->

Type linking was the first phase where we needed to handle multiple input files.
Previously, each of our phases accepted one file and output one file (for
the next stage and debugging).

Our compiler was modified in the following ways:

  * the `joosc` script accepts multiple parameters and calls each of the initial
    phases on all of them. Once the initial phases are over, we call
    `bin/compiler` and pass in all the resulting parse trees
  * `bin/compiler` reads all the parse trees, converts them into ASTs, then
    combines them into one big AST and runs the error checking code on the
    resulting AST.

We later made some changes to allow for parallel processing, which speeded up
our feedback cycle.

In our code, the hierarchy is represented as packages, subpackages, and types.
There is one package at the top (default package), which has as subpackages all
declared packages that are one level below (e.g. `java`). Each of those
subpackages will in turn have subpackages that are one level below them (so
`java` would have `lang` as one subpackage).

The subpackages are stored in a map-like structure, associated lists. This
allows for easy lookup without adding unnecessary complexity.

Each subpackage either has a `Package` or `Nothing`. For example, the `java`
subpackage has `Nothing` as its package, but `java.lang` has a `Package` which
contains all of the default compilation units and their types.

This hierarchy makes it easy to look up types: we simply need to go to the
default package and recursively descend the subpackages. Then we find the
compilation unit for the type using the last part of the compound name.

Of course, that requires the canonical name. So what we do before any types are
looked up is traverse the entire program and canonicalize all type names in
local/field declarations and in super/interface types. If the type cannot be
canonicalized due to a missing package or type declaration, it is treated as a
program error.

## Disambiguation

Disambiguation is something our compiler does not do fully yet.

When generating code, the function used will be `resolveInScope`. It receives a
name (and the context) and handles disambiguating it and classifying it as a
type, field, local, or package. It returns

What is missing is checking all imports to ensure that there are no conflicts.

Currently, we have the following two functions: (in
`src/lib/JoosCompiler/Ast/Utils.hs`)

  * `resolveTypeInProgram`
  * `resolvePackageFromProgram`

Those functions resolve to a type or a package only if that type or package
exists. They will be used in a disambiguation phase to check that no conflicts
occur.

The reason this is not finished yet is due to challenges we faced writing the
functions that resolve names. That was due to the fact that we don't have a
proper symbol table because the idea did not make much sense in Haskell (as it
disallows mutation). Instead, our resolve functions accept the program (a node
type which contains all packages) and a scope (or compilation unit where
appropriate) as the context and resolve the name using those.

## Type Checking

Type checking is done in `src/compiler/Linking/TypeChecking.hs`.

This is the first phase where we use our AST without the parse tree nodes. Type
checking is done by implementing the `Analysis` typeclass on our AST. The reason
we created this typeclass is to enable us to easily traverse expressions and
statements in our AST. Statements and expressions are different from the rest of
the program because they undergo heavy processing, and they don't look like a
typical Haskell tree.

The different forms statements and expressions can take are in
`src/lib/JoosCompiler/Ast/NodeTypes.hs`. The reason for the special treatment
they receive is that it is easier to represent them as abstract data types than
as a tree. For example, an addition expression would look something along the
lines of:

`BinaryOperation Add expression1 expression2`

If expressions were represented as trees, then it would be more similar to:

`Node (BinarryOperation Add) [expression1, expression2]`

where the list at the end is the children. While representing the expressions
and statements as trees would have made it simpler to deal with them in some
ways, it would have in fact been harder to do the kind of computation we need to
do since it would require relying on the children indices.

What the `Analysis` typeclass does is define a function for handling each
statement and expression. Then it recursively runs those functions as if we were
traversing a tree, but giving us the benefit of working with a datatype that
suits our usecase.

The type checking implements the `Analysis` typeclass functions appropriately so
that they traverse methods in our tree and return an error if one is found.

The mechanism for returning an error is the `Either` data type, which allows for
wrapping a result in either a `Left` or a `Right`. Conventionally, `Left`
represents an error. Thus our analysis code checks for error conditions and
returns an error string wrapped in a `Left` if one is found (short circuiting)
and `Right ()` otherwise. () is the void expression in Haskell.

Our type checking code currently fails many of the positive marmoset tests due
to the problems described above with our resolve functions. Anytime a name is
resolved, we receive an error which bubbles up and causes the whole program to
reported as erroneous.

## Reachability

<!--

Analysis class for Expressions so we can deal with them in a way similar to
trees

Analysis "instance" was used to implement all the rules

-->

Reachability checking also relies on the `Analysis` typeclass. The code is in
`src/compiler/StaticAnalysis/Reachability.hs` and `src/compiler/StaticAnalysis/Reachability3.hs`

# Challenges

# Testing


## Parallelized tests

Sequential: 126s

Parallel: 22.44s

Which is a 5.6x improvement
