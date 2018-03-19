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

<!--

Need to talk about
- we did it bit by bit and kept the original structure
- at the end we throw away the structure
- we gradually annotate the tree with more and more information
- one challenge was the fact that we have no pointers, so we had to ensure that
  we were not using stale data

-->

### Environment Building

<!--

A Scope is an environment in our code. Scopes store local declarations, parent
scopes, and the containing class.

We use scopes to resolve names to either local declarations or fields

-->

### Type Linking and Hierarchy Checking

<!--

- Dealing with multiple files
- How hierarchy is represented (packages, subpackages, types)
- Type canonicalization replaces all names (whether used as types in
  declarations or as compound names for fields)
- We have functions like resolveTypeInProgram, resolveInScope that allow us to
  look the declaration and get all the information we need

-->

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
