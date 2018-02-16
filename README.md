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

Definitions: list of token types and "regular expressions".

Input: file

Output: string of tokens

* Splits up the input into tokens
* Catches lexical errors
* Also checks that all byte are in the ASCII range (0 to 127). It is easiest to
  weed out non-ASCII characters from the start to simplify the
  scanner.

The regular expressions are expressed using an EDSL (embedded
domain-specific language) grammar . The grammar is based on the paper
"Monadic Parsing in Haskell" and a modernization of its implementation
(cited below).

The scanner reads through the source code, breaking it into tokens. It
finds the type of the token through greedy matching and backtracking
in the event of failure. If the scanner cannot find the token type
through backtracking, it returns a scanning error.

The scanner also returns a scanning error if non-ascii characters are
present or if it detects an invalid token (e.g illegal
keyword/operator). Some of the error detection is done after the
program is scanned.

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

* Detects context-sensitive errors in the programs through tree traversal
* Operates on a concrete syntax tree

In order to have the weeder working on time, it was implemented before
the AST building part was completed. Because of that, it operates on
the concrete syntax tree of the program.

### Stage 4: AST Building

Haskell

Input: parse tree

Output: Abstract Syntax Tree (AST)

* Converts the initial parse tree into a simpler AST

## Design Issues

### Resolving Parser Conflicts

TODO(ryan)

### Greedy matching causes Scanner to recognize invalid programs

Because of the style of the grammar used to represent the token types
in the scanner, greedy matching caused an issue where some invalid
programs were accepted.

An example is the invalid integer literal 010. The greedy matching
algorithm matched the integer 0 followed by the integer 10 -- when it
should have rejected the program. A workaround for this problem would
have been to complicate the grammar sufficiently in order to catch
those instances.

However, a simpler solution was implemented, which
was to check whether two consecutive `IntLiteral`s exist in the
program. This worked because spaces and comments were scanned as
tokens, so the only time where there were two consecutive
`IntLiteral`s is when an integer had a leading zero.

Another example of this occured with octal escapes inside
strings. `"\400"` is illegal in Joos. However, the scanner accepted
this as the StringLiteral composed of `"\4"` followed by `"00"`. This
problem has not been addressed yet, but will be fixed in the future
through a more complex grammar that simulates lookahead.

### Interoperability between languages

<!-- This might be too trivial to add here -->

## Testing

* Haskell unit tests
* positive/negative system tests

## Attribution

The parsing code in [src/haskell/Parsing.hs](src/haskell/Parsing.hs) is based on
the paper
[Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf). We
also adopted the improvements outlined in
[Revisiting 'Monadic Parsing in Haskell](http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/).
