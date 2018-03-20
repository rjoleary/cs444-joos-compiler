# Joos Compiler Design

* Ahmed Al-Sudani (a2alsuda)
* Ryan O'Leary (rj2olear)
* Xia Liu (x397liu)

## Code Structure

### Directory Structure

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

### Build System

In addition to building compiler, `MAKEFILE` contains a number of other useful
utilities:

    make                # Build the compiler
    make grammar        # Rebuild the parse table
    make test.a$n       # Run tests for assignment n
    make test.unit      # Run Haskell unit tests
    make test           # Run our own test files
    make zip            # Generate zip file for submitting to Marmoset
    make report         # Compile the report into a PDF
    make clean          # Delete intermediate files

## Phases in A2-A4

<!-- Don't forget to add a lot of technical details this time -->

### AST Generation

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

### Environment Building

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

### Type Linking and Hierarchy Checking

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



### Reachability

<!--

Analysis class for Expressions so we can deal with them in a way similar to
trees

Analysis "instance" was used to implement all the rules

-->

## Testing


### Parallelize tests

Sequential: 126s

Parallel: 22.44s

Which is a 5.6x improvement
