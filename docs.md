# Compiler Design

## Stage 0: Load file

* Also checks that all byte are in the ASCII range (0 to 127).

## Stage 1: Scanner

* Splits up the input into tokens
* Catches lexical errors

## Stage 2: Parser

* Generates a parse tree
* Checks that the input conforms to a Context-Free Grammar (CFG)

## Stage 3: Weeder

* Detects simple errors

## Stage 4: AST Building

* Converts the initial parse tree into a simpler Abstract Syntax Tree (AST)
