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
* `def/joos.cfg2`: Joos production rules
* `test/positive/`: Test inputs which are expected to pass
* `test/negative/`: Test inputs which are expected to fail
* `test/haskell/`: Unit tests written in Haskell which currently only
  test the Lexer stage
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
    make report         # Compile the report into a PDF
    make clean          # Delete intermediate files

## Stages

Stages are run in sequence via a shell script called "joosc". The parser is
written in Rust and the parse table generator is written in Java (provided by
the course website). All other components are written in Haskell.

Rust and Haskell were chosen because they provide type safety and
powerful pattern matching.

### Stage 1: Scanner

Haskell

Definitions: list of token types and "regular expressions".

Input: file

Output: string of tokens

* Splits the input into tokens
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

The scanner also returns a scanning error if non-ASCII characters are
present or if it detects an invalid token (e.g illegal
keyword/operator). Some of the error detection is done after the
program is scanned.

In addition to outputting the token types, the scanner also outputs
the start and end indices of all tokens. This makes it possible to
retrieve the lexeme in later stages and to generate detailed error
messages.

### Stage 2: Parser

Rust

Definitions: LR(1) oracle

Input: string of tokens

Output: rightmost derivation

* Bottom-up parse
* Checks that the input conforms to a Context-Free Grammar (CFG)

Switched from LR(1) to LALR(1). Early measurements showed the LALR grammar is
4x smaller without affecting the language.

The course provided jlalr tool was used to generate the parse table.

### Stage 3: Weeder

Haskell

Input: rightmost derivation

Output: parse tree (data structure in memory)

* Detects context-sensitive errors in the programs through tree traversal
* Operates on a concrete syntax tree

In order to have the weeder working on time, it was implemented before
the AST building part was completed. Because of that, it operates on
the concrete syntax tree of the program.

The weeder checks for errors by repeatedly traversing the tree to
check if it breaks a specific rule.

### Stage 4: AST Building

Haskell

Input: parse tree

Output: Abstract Syntax Tree (AST)

* Converts the initial parse tree into a simpler AST

## Design Issues

### Resolving Parser Conflicts

We used the grammar from the JLS2 with a few modifications. The production
rules can be found in `def/joos.cfg2`. The modifications were necessary to make
the grammar conflict-free, LALR(1) and easy to parse.

For example, JLS2 allows classes to contain a list of declarations which are
either `FieldDeclaration` or `MethodDeclaration`. Each declaration is prefixed
with a list of modifiers. The following subset of the grammar incurs a
reduce-reduce conflict:

    # JLS2
    (1) ClassMemberDeclaration -> FieldDeclaration
    (2) ClassMemberDeclaration -> MethodDeclaration
    (3) FieldDeclaration       -> FieldModifiers Type VariableDeclarator
    (4) MethodDeclaration      -> MethodModifiers Type MethodDeclarator MethodBody
    (5) FieldModifiers         -> FieldModifier
    (6) FieldModifiers         -> FieldModifiers FieldModifier
    (7) FieldModifier          -> public
    (8) FieldModifier          -> native
    (9) MethodModifiers        -> MethodModifier
    (10) MethodModifiers       -> MethodModifiers MethodModifer
    (11) MethodModifier        -> public

After pushing `public` onto the stack, the parser is unsure which reduce action
to apply. Should it reduce with (7) or (11)? Single token lookahead will not
help because both actions (3) and (4) have `Type` as the next token. There is
nothing to disambiguate. This is not LR(1).

To remove the conflict, we refactored the grammar by combining all modifiers
into the same set of production rules like so:

    # Joos
    (1) ClassMemberDeclaration -> FieldDeclaration
    (2) ClassMemberDeclaration -> MethodDeclaration
    (3) FieldDeclaration       -> Modifiers Type VariableDeclarator
    (4) MethodDeclaration      -> Modifiers Type MethodDeclarator MethodBody
    (5) Modifiers              -> Modifier
    (6) Modifiers              -> Modifiers Modifier
    (7) Modifiers              -> public
    (8) Modifiers              -> native

The grammar has been factored so that `public` will always have the following
derivation regardless of being in a field or method:

    Modifiers -> Modifier -> public

Unfortunately, the grammar is now weaker because it allows the `native` to be
applied to fields. Additional weeder rules account for this.

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

Another example of this occurred with octal escapes inside
strings. `"\400"` is illegal in Joos. However, the scanner accepted
this as the StringLiteral composed of `"\4"` followed by `"00"`. This
problem has not been addressed yet, but will be fixed in the future
through a more complex grammar that simulates lookahead.

### Interoperability between languages

Because of our choice to use multiple programming languages to
implement the compiler, we needed a method to communicate the output
of one stage to the next.

One option was to use shell pipes, but we opted to save the output of each
stage as a file. This makes it possible to use the output of a stage
in future stages whether they are run immediately after or later in
the pipeline.

## Testing

* Haskell unit tests
* positive/negative system tests

## Attribution

The parsing code in [lexer/haskell/Parsing.hs](src/haskell/Parsing.hs) is based on
the paper
[Monadic Parsing in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf). We
also adopted the improvements outlined in
[Revisiting 'Monadic Parsing in Haskell](http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/).
