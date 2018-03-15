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
import           Linking.TypeChecking
import           NameResolution.EnvironmentBuilding
import           NameResolution.HierarchyChecking
import           NameResolution.TypeLinking
import           Reachability
import           System.Environment
import           Data.Maybe

checkRules :: [AstRule] -> AstNode -> [String]
checkRules rules t = map getRuleString $ filter checkRule rules
  where
    checkRule :: AstRule -> Bool
    checkRule (_, predicate) = predicate t
    getRuleString :: AstRule -> String
    getRuleString (ruleString, _) = ruleString

main :: IO ()
main = do
  -- This booleans control if the compiler exists early based on the assignment number.
  testNum <- fmap (read . fromMaybe "999") (lookupEnv "TESTNUM")

  when (testNum > 1) $ do
    -- AST generation
    filenames <- getArgs
    taggedTrees <- mapM taggedTreeFromFile filenames
    let ast = cstsToAst taggedTrees
    putStrLn $ drawTree (fmap show ast)

    -- Environment building & type linking
    let failedRules = checkRules (environmentBuildingRules ++ typeLinkingRules) ast
    when (length failedRules > 0) $ exitError $ "Failed to pass those rules:\n" ++ intercalate "\n" failedRules

    -- Hierarchy checking
    let hierarchy = checkHierarchy ast
    case hierarchy of
      Right _  -> return ()
      Left err -> exitError err

    when (testNum > 2) $ do
      -- Type checking
      case (checkTypes ast) of
        Right _ -> return ()
        Left err -> exitError err

    when (testNum > 3) $ do
      -- Return / reachability
      case (checkReturnAndReachability ast) of
        [] -> return ()
        errors -> exitError $ intercalate "\n" errors

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
