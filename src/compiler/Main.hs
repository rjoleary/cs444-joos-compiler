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
import           JoosCompiler.Ast.Transformers.Types
import           NameResolution.EnvironmentBuilding
import           NameResolution.HierarchyChecking
import           NameResolution.TypeLinking
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

  -- AST generation
  filenames <- getArgs
  taggedTrees <- mapM taggedTreeFromFile filenames
  let ast = cstsToAst taggedTrees
  --putStrLn $ drawTree (fmap show ast)

  -- Environment building & type linking
  let failedRules = checkRules (environmentBuildingRules ++ typeLinkingRules) ast
  --putStrLn $ intercalate "\n" failedRules
  when (length failedRules > 0) $ exitError "See failed rules above"

  -- Simplify the AST
  let ast2 = asTree $ asAst ast
  putStrLn $ drawTree (fmap show ast2)

  -- Hierarchy checking
  let hierarchy = checkHierarchy ast2
  case hierarchy of
    Right _  -> return ()
    Left err -> exitError err


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
