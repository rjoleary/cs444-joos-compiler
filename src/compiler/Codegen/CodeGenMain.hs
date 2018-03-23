module Codegen.CodeGenMain
  ( codeGenMain
  ) where

import Data.Int
import JoosCompiler.Ast
import Codegen.Mangling
import Codegen.X86

-- Generates asm for the main file which is the entrypoint and initializes
-- static variables.
codeGenMain :: AstNode -> Either String (Asm ())
codeGenMain ast = Right $ do
  comment "This is the entrypoint"
  global "_start"
  label "_start"
  exitSyscall

exitSyscall :: Asm ()
exitSyscall = do
  mov Eax (I 1)
  mov Ebx (I 123)
  int (I 0x80)
