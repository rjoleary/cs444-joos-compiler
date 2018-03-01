Assignment 2
============

The previous README has been moved to
[docs/report1.md](docs/report1.md).

### Assignment 2 plan

I (Ahmed) put some ideas for how we can solve the different parts of
A2 in [a document](docs/planning.md). Feel free to modify if you have
something better in mind.

Several items are marked as TODO -- usually because I need to consult
JLS.

### AST Summary

The AST generation code lives in
[src/lib/haskell/JoosCompiler/Ast/](src/lib/haskell/JoosCompiler/Ast/). The
AST we currently have should be sufficient for all of A2.

An example of AST usage is in [src/ast/Main.hs](src/ast/Main.hs).

### Debugging

If you need to debug your code or use a REPL, you can run `make
ghci`.

This snippet worked for me when I was testing things

```haskell
:l src/lib/haskell/JoosCompiler/Ast.hs

:set prompt "> "

import Data.Tree
import JoosCompiler.Ast.Utils
import JoosCompiler.TokenTypeConstants

classname <- readFile "test/joos_classname.txt"
source <- readFile "test/joos_input.txt"
tokens <- readFile "test/joos_tokens.txt"
contents <- readFile "test/joos_parse.txt"
let tree = treeify contents
let taggedTree = tagTree tree tokens source
let ast = cstToAst taggedTree
putStr $ drawTree $ fmap show ast
```
