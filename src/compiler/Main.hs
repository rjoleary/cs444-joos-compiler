-- This file only tests conversion to an AST. The idea is to test the
-- conversion here by outputting the result and import the AST
-- conversion facilities where they're needed
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Treeify
import           System.Environment
import           SymbolTable

main :: IO ()
main = do
  filenames <- getArgs
  astForest <- mapM astFromFile filenames
  let ast = Node (wholeProgramTransformer astForest) (astForest)
  putStrLn $ drawTree (fmap show ast)

  let symbolTable = createSymbolTable (rootLabel ast)
  case symbolTable of
    Right symbolTable' -> putStrLn (show symbolTable')
    Left err           -> putStrLn err

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
