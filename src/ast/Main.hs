-- This file only tests conversion to an AST. The idea is to test the
-- conversion here by outputting the result and import the AST
-- conversion facilities where they're needed
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Treeify

main :: IO ()
main = do
  source <- readFile "test/joos_input.txt"
  tokens <- readFile "test/joos_tokens.txt"
  contents <- readFile "test/joos_parse.txt"
  let tree = treeify contents
  let taggedTree = tagTree tree tokens source
  let ast = cstToAst taggedTree
  putStrLn $ drawTree (fmap show ast)
