module Codegen.X86
  ( Asm
  , Reg(..)
  , add
  , sub
  , imul
  , idiv
  ) where

import Data.Int
import Codegen.Mangling

data Asm a = Asm [String]
data Reg = Eax | Ebx | Ecx | Edx | Esp | Ebp | Esi | Edi | Eip
data Addr = Addr Reg | Offset Reg Int32

instance Show (Asm a) where
  show (Asm x) = unlines x

instance Show Reg where
  show Eax = "eax"
  show Ebx = "ebx"
  show Ecx = "ecx"
  show Edx = "edx"
  show Esp = "esp"
  show Ebp = "ebp"
  show Esi = "esi"
  show Edi = "edi"
  show Eip = "eip"

instance Show Addr where
  show (Addr x)     = "[" ++ show x ++ "]"
  show (Offset x y) = "[" ++ show x ++ (if y >= 0 then "+" else "") ++ show y ++ "]"

-- Argument on the left side
class (Show a) => LArg a
instance LArg Reg
instance LArg Addr

-- Argument on the right side
class (Show a) => RArg a
instance RArg Reg
instance RArg Addr
instance RArg Int32
-- TODO: instance (Mangleable a) => RArg Mangleable

raw :: String -> Asm ()
raw x = Asm [x]

-- Generic instruction taking one argument.
generic1 :: (RArg b) => String -> b -> Asm ()
generic1 op x = raw (op ++ " " ++ show x ++ ";")

-- Generic instruction taking two arguments.
generic2 :: (LArg a, RArg b) => String -> a -> b -> Asm ()
generic2 op x y = raw (op ++ " " ++ show x ++ ", " ++ show y ++ ";")

-- add eax, ebx;
--   eax <- eax + ebx
add :: (LArg a, RArg b) => a -> b -> Asm ()
add = generic2 "add"

-- sub eax, ebx;
--   sub <- eax - ecx
sub :: (LArg a, RArg b) => a -> b -> Asm ()
sub = generic2 "sub"

-- imul eax, ebx;
--   edx:eax <- eax * ebx
imul :: (LArg a, RArg b) => a -> b -> Asm ()
imul = generic2 "mul"

-- idiv eax;
--   eax <- edx:eax / ebx
--   edx <- edx:eax % ebx
idiv :: (RArg b) => b -> Asm ()
idiv = generic1 "idiv"

-- mov eax, 22;
-- mov eax, Label;
-- mov eax, [label]; TODO
-- mov eax, [esp+8];
-- mov [eax], ebx;
mov :: (LArg a, RArg b) => a -> b -> Asm ()
mov = generic2 "mov"

jmp :: (RArg b) => b -> Asm ()
jmp = generic1 "jmp"

push :: (RArg b) => b -> Asm ()
push = generic1 "push"

pop :: (RArg b) => b -> Asm ()
pop = generic1 "push"

-- TODO: cmp, je, jne, jg, jge, je, jle, ja, jb, jae, jbe
