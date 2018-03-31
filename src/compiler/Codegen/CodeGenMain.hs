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
  comment "TODO: re-enable when this does not cause segfault"
  {-mapM_ (\x -> extern (Init x) >> call (L . mangle . Init $ x)) (let
    decl CompilationUnit{typeDecl=Just x} = [x]
    decl _                                = []
    types = foldl (++) [] . map decl . programCus $ wp
    classes = filter (not . isInterface) types
    in classes)
    -}
  space

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

  memclearFunction
  space

-- Exit with the value stored in eax.
exitSyscall :: Asm ()
exitSyscall = do
  mov Ebx Eax
  mov Eax (I 1)
  int (I 0x80)

-- Create the memclear function.
memclearFunction :: Asm ()
memclearFunction = do
  comment "eax is the first address."
  comment "ebx is the size in bytes."
  comment "Both registers are modified."
  global "memclear"
  label "memclear"
  indent $ do
    add Ebx Eax
    label "memclear_loop"
    cmp Eax Ebx
    je (L "memclear_return")
    movByte (Addr Eax) (I 0)
    add Eax (I 1)
    jmp (L "memclear")
    label "memclear_return"
    ret
