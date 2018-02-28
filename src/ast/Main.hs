-- This file only tests conversion to an AST. The idea is to test the
-- conversion here by outputting the result and import the AST
-- conversion facilities where they're needed
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Treeify
import           System.Environment

main :: IO ()
main = do
  filenames <- getArgs
  astForest <- mapM astFromFile filenames
  putStrLn $ drawForest (fmap (fmap show) astForest)

stripSuffix :: String -> String
stripSuffix ".java" = ""
stripSuffix (x:xs)  = x : (stripSuffix xs)
stripSuffix ""      = error "Filename does not end in .java"

astFromFile :: String -> IO AstNode
astFromFile filename = do
  let basename = stripSuffix filename
  source <- readFile filename
  tokens <- readFile (basename ++ ".tokens")
  parse <- readFile (basename ++ ".parse")
  let tree = treeify parse
  let taggedTree = tagTree tree tokens source
  let ast = cstToAst taggedTree
  return ast
