# Compiler Design

## Stages

Stages are run serial via a shell script called "joosc". Everything up to and
including the parser is written in Rust. The weeder and all the following
stages are written in Haskell.

### Stage 0: Load file

Rust

Input: filename

Output: string of characters

* Also checks that all byte are in the ASCII range (0 to 127). It is easiest to
  weed out non-ASCII characters from the start to simplify the scanner.

### Stage 1: Scanner

Rust

Definitions: list of regular expressions and lexeme ids

Input: string of characters

Output: string of tokens

* Splits up the input into tokens
* Catches lexical errors

### Stage 2: Parser

Rust

Definitions: LR(1) oracle

Input: string of tokens

Output: rightmost derivation

* Bottom-up parse
* Checks that the input conforms to a Context-Free Grammar (CFG)

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

## Directory layout

* src: Rust source code
* def: Grammars and other definition files
* test: Files for testing
