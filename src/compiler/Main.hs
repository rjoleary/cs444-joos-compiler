-- This file only tests conversion to an AST. The idea is to test the
-- conversion here by outputting the result and import the AST
-- conversion facilities where they're needed
import           AstRule
import           Control.Monad
import           Data.List
import           Data.Tree
import           JoosCompiler.Ast
import           JoosCompiler.Exit
import           JoosCompiler.Treeify
import           NameResolution.EnvironmentBuilding
import           SymbolTable
import           System.Environment

checkRules :: [AstRule] -> AstNode -> [String]
checkRules rules t = map getRuleString $ filter checkRule rules
  where
    checkRule :: AstRule -> Bool
    checkRule (_, predicate) = predicate t
    getRuleString :: AstRule -> String
    getRuleString (ruleString, _) = ruleString

main :: IO ()
main = do
  filenames <- getArgs
  taggedTrees <- mapM taggedTreeFromFile filenames
  let ast = cstsToAst taggedTrees
  putStrLn $ drawTree (fmap show ast)
  let symbolTable = createSymbolTable (rootLabel ast)
  case symbolTable of
    Right symbolTable' -> putStrLn (show symbolTable')
    Left err           -> putStrLn err
  let failedRules = checkRules typeLinkingRules ast
  putStrLn $ intercalate "\n" failedRules
  when (length failedRules > 0) $ exitError "See failed rules above"

stripSuffix :: String -> String
stripSuffix ".java" = ""
stripSuffix (x:xs)  = x : (stripSuffix xs)
stripSuffix ""      = error "Filename does not end in .java"

taggedTreeFromFile :: String -> IO TaggedParseTree
taggedTreeFromFile filename = do
  let basename = stripSuffix filename
  source <- readFile filename
  tokens <- readFile (basename ++ ".tokens")
  parse <- readFile (basename ++ ".parse")
  let tree = treeify parse
  let taggedTree = tagTree tree tokens source
  return taggedTree
