# Joos Compiler Design

* Ahmed Al-Sudani (a2alsuda)
* Ryan O'Leary (rj2olear)
* Xia Liu (x397liu)

## Code Structure

### Directory Structure

* `src/haskell/`: Haskell source code for the lexer
* `src/rust/`: Rust source code for the parse engine
* `src/java/`: Java source code for the parse generator
* `src/weeder/`: Haskell source code for the weeder
* `src/ast/`: Haskell source code for the AST
* `def/`: Joos grammar and other definition files
* `test/positive/`: Test inputs which are expected to pass
* `test/negative/`: Test inputs which are expected to fail
* `test/haskell/`: Unit tests written in Haskell which test only the Lexer stage
* `MAKEFILE`: Builds compiler, build documentation, runs tests, etc...
* `joosc`: Builds

### Build System

In addition to building compiler, `MAKEFILE` contains a number of other useful
utilities:

    make                # Build the compiler
    make grammar        # Rebuild the parse table
    make test           # Run all tests
    make test.positive  # Run the negative tests
    make test.negative  # Run the positive tests
    make test.unit      # Run Haskell unit tests
    make zip            # Generate zip file for submitting to Marmoset
    make clean          # Delete intermediate files

## Stages

Stages are run in sequence via a shell script called "joosc". The parser is
written in Rust and the parse table generator is written in Java (provided by
the course website). All other component are written in Haskell.

### Stage 1: Scanner

Haskell

Definitions: list of regular expressions and lexeme ids

Input: file

Output: string of tokens

* Splits up the input into tokens
* Catches lexical errors

* Also checks that all byte are in the ASCII range (0 to 127). It is easiest to
  weed out non-ASCII characters from the start to simplify the scanner.

### Stage 2: Parser

Rust

Definitions: LR(1) oracle

Input: string of tokens

Output: rightmost derivation

* Bottom-up parse
* Checks that the input conforms to a Context-Free Grammar (CFG)

Switched from LR(1) to LALR(1). Early measurements showed the LALR grammar is
4x smallar without affecting the language.

The course provided jlalr tool was used to generate generate the parse table.

### Stage 3: Weeder

Haskell

Input: rightmost derivation

Output: parse tree (data structure in memory)

* Detects simple errors

### Stage 4: AST Building

Haskell

Input: parse tree

Output: Abstract Syntax Tree (AST)

* Converts the initial parse tree into a simpler AST

## Design Issues

### Resolving Parser Conflicts

TODO(ryan)

## Testing

* Haskell unit tests
* positive/negative system tests

## Attribution

The parsing code in [src/haskell/Parsing.hs](src/haskell/Parsing.hs) is based on
the paper
[Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf). We
also adopted the improvements outlined in
[Revisiting 'Monadic Parsing in Haskell](http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/).