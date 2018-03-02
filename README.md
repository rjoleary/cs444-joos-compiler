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

We have chosen to keep the structure of the parse tree even after AST
generation. This allows us to gradually convert parts of the tree and
gives us access to the parse tree where it's needed.

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
[src/lib/haskell/JoosCompiler/Ast/Transformers](src/lib/haskell/JoosCompiler/Ast/Transformers).
The correct transformer is picked by `getTransformer` in
[Core.hs](src/lib/haskell/JoosCompiler/Ast/Core.hs), which decides
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
  [src/lib/haskell/JoosCompiler/Ast/NodeTypes.hs](src/lib/haskell/JoosCompiler/Ast/NodeTypes.hs). This
  can be something like `Type`, `ClassDeclaration`, `Block`, etc.

* `AstWrapper`, which wraps the node types in `NodeTypes.hs`. It can
  also wrap a `TaggedToken`, which is how we keep the parse tree

* `AstNode :: Tree AstWrapper`

### Working with the AST

Importing `JoosCompiler.Ast` should be sufficient, but if you find
that you are missing some types, they might be in
`JoosCompiler.Ast.NodeTypes` or
`JoosCompiler.Ast.Transformers.Types`.

There are also types you might need in `JoosCompiler.Treeify` (ones
relevant to the parse tree).

`JoosCompiler.TreeUtils` will certainly come in handy too. It has
functions of various signatures for finding nodes in trees.

If you are searching for an AST node of a specific type (e.g. a block
or a method declaration), you will probably want to use one of the
functions in `TreeUtils` and the `AstWrapper -> Bool` functions in
[`JoosCompiler.Ast.Transformers.Types`](src/lib/haskell/JoosCompiler/Ast/Transformers/Types.hs). Feel
free to add more if you need to.

### Debugging

If you need to debug your code or use a REPL, you can run `make
ghci`.

This snippet worked for me when I was testing things

```haskell
:l src/compiler/Main.hs

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
let ast = cstsToAst [taggedTree]
putStr $ drawTree $ fmap show ast
```
