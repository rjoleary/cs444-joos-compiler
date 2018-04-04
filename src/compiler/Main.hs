-- This file only tests conversion to an AST. The idea is to test the
-- conversion here by outputting the result and import the AST
-- conversion facilities where they're needed
import           AstRule
import           Control.Monad
import           Data.Either
import           Data.List
import           Data.Tree
import           Flow
import           JoosCompiler.Ast
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.Exit
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils
import           Linking.TypeChecking
import           NameResolution.EnvironmentBuilding
import           NameResolution.HierarchyChecking
import           NameResolution.TypeLinking
import           StaticAnalysis.Reachability
import           StaticAnalysis.Reachability3
import           Codegen.CodeGenMain
import           Codegen.CodeGenType
import           Codegen.Mangling
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
    let ast@(Node (AstWholeProgram program@WholeProgram{programCus = units}) unitNodes) = cstsToAst taggedTrees
    putStrLn $ drawTree $ fmap show $ asTree $ wrap $ program

    let typeDecls = map typeDecl units
                    |> catMaybes

    -- Environment building & type linking
    let failedRules = checkRules (environmentBuildingRules ++ typeLinkingRules) ast
    when (length failedRules > 0) $ exitError $ "Failed to pass those rules:\n" ++ intercalate "\n" failedRules

    -- Hierarchy checking
    let hierarchy = checkHierarchy ast
    case hierarchy of
      Right _  -> return ()
      Left err -> exitError err

    when (testNum > 3) $ do
      -- 3rd reachability rule
      case (checkReachability3 ast) of
        Right _ -> return ()
        Left err -> exitError err

      -- Return / reachability
      case (checkReachability ast) of
        Right _ -> return ()
        Left err -> exitError err

    when (testNum > 4) $ do
      -- Code generation
      case (codeGenMain program) of
        Right asm -> writeFile "output/main.s" (show asm)
        Left err  -> exitError err

      mapM_ (\t -> case (codeGenType program t) of
          Right asm -> writeFile (asmFileName t) (show asm)
          Left err  -> exitError err)
        typeDecls

-- Create a filename for the assembly of the given type.
asmFileName :: TypeDeclaration -> String
asmFileName t = "output/" ++ mangle t ++ ".s"

checkUnitTypes :: WholeProgram -> AstNode -> Either String ()
checkUnitTypes program unitNode@(Node (AstCompilationUnit unit) _) =
  checkTypes program unit unitNode
checkUnitTypes _ _ = error "Wrong node type in checkUnitTypes"

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
