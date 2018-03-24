{-# LANGUAGE FlexibleInstances #-}
module Codegen.X86
  ( Asm
  , Reg(..)
  , Addr(..)
  , I(..)
  , comment
  , label
  , global

  -- generic 1
  , idiv
  , jmp
  , push
  , pop
  , int
  , dd
  , je
  , jne
  , jg
  , jge
  , jle
  , ja
  , jb
  , jae
  , jbe


  -- generic 2
  , add
  , sub
  , imul
  , or
  , and
  , mov
  , cmp

  ) where

import Prelude hiding (and, or)
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
  pure x = Asm []

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

label :: (Mangleable a) => a -> Asm ()
label m = raw (mangle m ++ ":")

global :: (Mangleable a) => a -> Asm ()
global m = raw ("global " ++ mangle m)

-- Generic instruction taking zero arguments.
generic0 :: String -> Asm ()
generic0 op = raw ("  " ++ op ++ ";")

-- Generic instruction taking one argument.
generic1 :: (Arg b) => String -> b -> Asm ()
generic1 op x = raw ("  " ++ op ++ " " ++ showArg x ++ ";")

idiv :: Arg a => a -> Asm ()
jmp  :: Arg a => a -> Asm ()
push :: Arg a => a -> Asm ()
pop  :: Arg a => a -> Asm ()
int  :: Arg a => a -> Asm ()
dd   :: Arg a => a -> Asm ()
je   :: Arg a => a -> Asm ()
jne  :: Arg a => a -> Asm ()
jg   :: Arg a => a -> Asm ()
jge  :: Arg a => a -> Asm ()
jle  :: Arg a => a -> Asm ()
ja   :: Arg a => a -> Asm ()
jb   :: Arg a => a -> Asm ()
jae  :: Arg a => a -> Asm ()
jbe  :: Arg a => a -> Asm ()

idiv = generic1 "idiv"
jmp  = generic1 "jmp"
push = generic1 "push"
pop  = generic1 "pop"
int  = generic1 "int"
dd   = generic1 "dd"
je   = generic1 "je"
jne  = generic1 "jne"
jg   = generic1 "jg"
jge  = generic1 "jge"
jle  = generic1 "jle"
ja   = generic1 "ja"
jb   = generic1 "jb"
jae  = generic1 "jae"
jbe  = generic1 "jbe"

-- Generic instruction taking two arguments.
generic2 :: (Arg a, Arg b) => String -> a -> b -> Asm ()
generic2 op x y = raw ("  " ++ op ++ " " ++ showArg x ++ ", " ++ showArg y ++ ";")

add  :: (Arg a, Arg b) => a -> b -> Asm ()
sub  :: (Arg a, Arg b) => a -> b -> Asm ()
imul :: (Arg a, Arg b) => a -> b -> Asm ()
or   :: (Arg a, Arg b) => a -> b -> Asm ()
and  :: (Arg a, Arg b) => a -> b -> Asm ()
mov  :: (Arg a, Arg b) => a -> b -> Asm ()
cmp  :: (Arg a, Arg b) => a -> b -> Asm ()

add  = generic2 "add"
sub  = generic2 "sub"
imul = generic2 "mul"
or   = generic2 "or"
and  = generic2 "and"
mov  = generic2 "mov"
cmp  = generic2 "cmp"
