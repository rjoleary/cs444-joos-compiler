{-# LANGUAGE FlexibleInstances #-}
module Codegen.X86
  ( Asm
  , Reg(..)
  , Addr(..)
  , I(..)
  , comment
  , add
  , sub
  , imul
  , idiv
  , mov
  , jmp
  , push
  , pop
  , int
  , label
  , global
  , dd
  ) where

import Data.Int
import Codegen.Mangling

data Asm a = Asm [String]
data Reg = Eax | Ebx | Ecx | Edx | Esp | Ebp | Esi | Edi | Eip
data Addr = Addr Reg | Offset Reg Int32
data I = I Int32
data L a = L a

instance Monoid (Asm a) where
  mempty = Asm []
  mappend (Asm x) (Asm y) = Asm (x ++ y)

instance Functor Asm where

instance Applicative Asm where

instance Monad Asm where
  (Asm x) >> (Asm y) = Asm (x ++ y)

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

instance Show I where
  show (I x) = show x

-- Argument types
class Arg a where
  showArg :: a -> String

instance Arg Reg where
  showArg x = show x

instance Arg Addr where
  showArg x = show x

instance Arg I where
  showArg x = show x

instance (Mangleable a) => Arg (L a) where
  showArg (L x) = mangle x

-- Instructions

raw :: String -> Asm ()
raw x = Asm [x]

comment :: String -> Asm ()
comment x = Asm ["; " ++ x]

-- Generic instruction taking one argument.
generic1 :: (Arg b) => String -> b -> Asm ()
generic1 op x = raw (op ++ " " ++ showArg x ++ ";")

-- Generic instruction taking two arguments.
generic2 :: (Arg a, Arg b) => String -> a -> b -> Asm ()
generic2 op x y = raw (op ++ " " ++ showArg x ++ ", " ++ showArg y ++ ";")

-- add eax, ebx;
--   eax <- eax + ebx
add :: (Arg a, Arg b) => a -> b -> Asm ()
add = generic2 "  add"

-- sub eax, ebx;
--   sub <- eax - ecx
sub :: (Arg a, Arg b) => a -> b -> Asm ()
sub = generic2 "  sub"

-- imul eax, ebx;
--   edx:eax <- eax * ebx
imul :: (Arg a, Arg b) => a -> b -> Asm ()
imul = generic2 "  mul"

-- idiv eax;
--   eax <- edx:eax / ebx
--   edx <- edx:eax % ebx
idiv :: (Arg b) => b -> Asm ()
idiv = generic1 "  idiv"

-- mov eax, 22;
-- mov eax, Label;
-- mov eax, [label]; TODO
-- mov eax, [esp+8];
-- mov [eax], ebx;
mov :: (Arg a, Arg b) => a -> b -> Asm ()
mov = generic2 "  mov"

jmp :: (Arg b) => b -> Asm ()
jmp = generic1 "  jmp"

push :: (Arg b) => b -> Asm ()
push = generic1 "  push"

pop :: (Arg b) => b -> Asm ()
pop = generic1 "  pop"

int :: (Arg b) => b -> Asm ()
int = generic1 "  int"

-- TODO: cmp, je, jne, jg, jge, je, jle, ja, jb, jae, jbe

label :: (Mangleable a) => a -> Asm ()
label m = raw (mangle m ++ ":")

global :: (Mangleable a) => a -> Asm ()
global m = raw ("global " ++ mangle m)

dd :: (Arg a) => a -> Asm ()
dd = generic1 "dd"
