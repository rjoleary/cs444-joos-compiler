module Codegen.CodeGenMain
  ( codeGenMain
  ) where

import Data.Int
import Data.Maybe
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes
import Codegen.Mangling
import Codegen.X86

-- Generates asm for the main file which is the entrypoint and initializes
-- static variables.
codeGenMain :: WholeProgram -> Either String (Asm ())
codeGenMain wp = Right $ do
  comment "This is the entrypoint"
  global "_start"
  label "_start"
  space

  comment "Initialize static fields"
  push Ebx
  push Edi
  push Esi
  mapM_ (\x -> extern (Init x) >> call (L . mangle . Init $ x)) (let
    decl CompilationUnit{typeDecl=Just x} = [x]
    decl _                                = []
    types = foldl (++) [] . map decl . programCus $ wp
    classes = filter (not . isInterface) types
    in classes)
  space
  pop Esi
  pop Edi
  pop Ebx

  comment "Call the test method, then exit"
  (let
    -- These are given based on the a5 spec, so empty lists are not expected.
    head (x:_) = x
    head _     = error "Expected first class to be the test class"
    firstClass = head $ maybeToList $ typeDecl $ head (programCus wp)
    isTestMethod x = methodName x == "test" && Static `elem` (methodModifiers x)
    testMethod = head . filter isTestMethod $ methods firstClass
    in extern testMethod >> call (L . mangle $ testMethod))
  exitSyscall
  space

-- Exit with the value stored in eax.
exitSyscall :: Asm ()
exitSyscall = do
  mov Ebx Eax
  mov Eax (I 1)
  int (I 0x80)
