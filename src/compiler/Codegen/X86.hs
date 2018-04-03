{-# LANGUAGE FlexibleInstances #-}
module Codegen.X86
  ( Asm
  , Reg(..)
  , Addr(..)
  , I(..)
  , L(..)
  , indent
  , uniqueLabel
  , raw
  , comment
  , space
  , label
  , global
  , extern
  , externIfRequired

  -- generic 0
  , nop
  , ret

  -- generic 1
  , idiv
  , jmp
  , call
  , push
  , pop
  , int
  , dd
  , je
  , jne
  , jg
  , jge
  , jl
  , jle
  , ja
  , jb
  , jae
  , jbe
  , seta
  , setae
  , setb
  , setbe
  , setc
  , sete
  , setg
  , setge
  , setl
  , setle
  , setna
  , setnae
  , setnb
  , setnbe
  , setnc
  , setne
  , setng
  , setnge
  , setnl
  , setnle
  , setno
  , setnp
  , setns
  , setnz
  , seto
  , setp
  , setpe
  , setpo
  , sets
  , setz

  -- generic 2
  , add
  , sub
  , imul
  , or
  , and
  , mov
  , movsx
  , movzx
  , movByte
  , movWord
  , movDword
  , cmp
  , shr
  , shl

  ) where

import Prelude hiding (and, or)
import Data.Int
import Data.List hiding (and, or)
import Codegen.Mangling
import JoosCompiler.Ast.NodeTypes

data AsmState = AsmState { code :: [String], counter :: Integer }
data Asm a = Asm (AsmState -> (AsmState, a))
data Reg =
  Eax | Ebx | Ecx | Edx |     -- general purpose 32-bit
  Ax | Bx | Cx | Dx |         -- general purpose 16-bit
  Al | Bl | Cl | Dl |         -- general purpose 8-bit
  Esp | Ebp | Esi | Edi | Eip -- special meaning
data Addr = Addr Reg | AddrOffset Reg Int32
data I = I Int32
data L a = L a

instance Monoid a => Monoid (Asm a) where
  mempty = pure mempty
  mappend x y = x >> y

instance Functor Asm where

instance Applicative Asm where
  pure x = Asm $ \y -> (y, x)

instance Monad Asm where
  return x = pure x
  Asm f >>= g = Asm $ \x -> let (y, z) = f x; Asm h = g z in h y

instance Show (Asm a) where
  show (Asm f) = unlines . code . fst $ f AsmState{ code = [], counter = 0 }

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
  show Ax  = "ax"
  show Bx  = "bx"
  show Cx  = "cx"
  show Dx  = "dx"
  show Al  = "al"
  show Bl  = "bl"
  show Cl  = "cl"
  show Dl  = "dl"

instance Show Addr where
  show (Addr x)         = "[" ++ show x ++ "]"
  show (AddrOffset x y) = "[" ++ show x ++ (if y >= 0 then "+" else "") ++ show y ++ "]"

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

-- Indent a block of assembly.
indent :: Asm a -> Asm a
indent (Asm f) = Asm $ \x ->
  let (y, z) = f AsmState{code=[], counter=counter x} in (indentCode x y, z)
  where
    indentCode :: AsmState -> AsmState -> AsmState
    indentCode x y = y{ code = code x ++ map ("  "++) (code y) }

-- Returns a unique label.
uniqueLabel :: Asm String
uniqueLabel = do
  counter <- getCounter
  setCounter (counter + 1)
  return ("label" ++ show counter)

getCounter :: Asm Integer
getCounter = Asm $ \x -> (x, counter x)

setCounter :: Integer -> Asm ()
setCounter x = Asm $ \y -> (y{ counter = x }, ())

-- Instructions

raw :: String -> Asm ()
raw x = Asm $ \y -> (y{ code = code y ++ [x] }, ())

comment :: String -> Asm ()
comment x = raw ("; " ++ x)

space :: Asm ()
space = raw ""

label :: (Mangleable a) => a -> Asm ()
label m = raw (mangle m ++ ":")

global :: (Mangleable a) => a -> Asm ()
global m = raw ("global " ++ mangle m ++ ";")

extern :: (Mangleable a) => a -> Asm ()
extern m = raw ("extern " ++ mangle m ++ ";")

-- You cannot extern something already defined in the same file/class.
-- This only adds an extern if the label is not part of this file.
externIfRequired :: (Mangleable a) => TypeDeclaration -> a -> Asm ()
externIfRequired decl m =
  if (stripPrefix $ mangle decl) `isPrefixOf` (stripPrefix $ mangle m)
  then return ()
  else extern m
  where
    stripPrefix ('C':'l':'a':'s':'s':'$':xs)     = xs
    stripPrefix ('F':'i':'e':'l':'d':'$':xs)     = xs
    stripPrefix ('M':'e':'t':'h':'o':'d':'$':xs) = xs
    stripPrefix xs = error $ "Do not use externIfRequired on this type " ++ xs

-- Generic instruction taking zero arguments.
generic0 :: String -> Asm ()
generic0 op = raw (op ++ ";")

nop :: Asm ()
nop = generic0 "nop"

ret :: Asm ()
ret = generic0 "ret"

-- Generic instruction taking one argument.
generic1 :: (Arg b) => String -> b -> Asm ()
generic1 op x = raw (op ++ " " ++ showArg x ++ ";")

idiv   :: Arg a => a -> Asm ()
jmp    :: Arg a => a -> Asm ()
call   :: Arg a => a -> Asm ()
push   :: Arg a => a -> Asm ()
pop    :: Arg a => a -> Asm ()
int    :: Arg a => a -> Asm ()
dd     :: Arg a => a -> Asm ()
je     :: Arg a => a -> Asm ()
jne    :: Arg a => a -> Asm ()
jg     :: Arg a => a -> Asm ()
jge    :: Arg a => a -> Asm ()
jl     :: Arg a => a -> Asm ()
jle    :: Arg a => a -> Asm ()
ja     :: Arg a => a -> Asm ()
jb     :: Arg a => a -> Asm ()
jae    :: Arg a => a -> Asm ()
jbe    :: Arg a => a -> Asm ()
seta   :: Arg a => a -> Asm ()
setae  :: Arg a => a -> Asm ()
setb   :: Arg a => a -> Asm ()
setbe  :: Arg a => a -> Asm ()
setc   :: Arg a => a -> Asm ()
sete   :: Arg a => a -> Asm ()
setg   :: Arg a => a -> Asm ()
setge  :: Arg a => a -> Asm ()
setl   :: Arg a => a -> Asm ()
setle  :: Arg a => a -> Asm ()
setna  :: Arg a => a -> Asm ()
setnae :: Arg a => a -> Asm ()
setnb  :: Arg a => a -> Asm ()
setnbe :: Arg a => a -> Asm ()
setnc  :: Arg a => a -> Asm ()
setne  :: Arg a => a -> Asm ()
setng  :: Arg a => a -> Asm ()
setnge :: Arg a => a -> Asm ()
setnl  :: Arg a => a -> Asm ()
setnle :: Arg a => a -> Asm ()
setno  :: Arg a => a -> Asm ()
setnp  :: Arg a => a -> Asm ()
setns  :: Arg a => a -> Asm ()
setnz  :: Arg a => a -> Asm ()
seto   :: Arg a => a -> Asm ()
setp   :: Arg a => a -> Asm ()
setpe  :: Arg a => a -> Asm ()
setpo  :: Arg a => a -> Asm ()
sets   :: Arg a => a -> Asm ()
setz   :: Arg a => a -> Asm ()

idiv   = generic1 "idiv"
jmp    = generic1 "jmp"
call   = generic1 "call"
push   = generic1 "push"
pop    = generic1 "pop"
int    = generic1 "int"
dd     = generic1 "dd"
je     = generic1 "je"
jne    = generic1 "jne"
jg     = generic1 "jg"
jge    = generic1 "jge"
jl     = generic1 "jl"
jle    = generic1 "jle"
ja     = generic1 "ja"
jb     = generic1 "jb"
jae    = generic1 "jae"
jbe    = generic1 "jbe"
seta   = generic1 "seta"
setae  = generic1 "setae"
setb   = generic1 "setb"
setbe  = generic1 "setbe"
setc   = generic1 "setc"
sete   = generic1 "sete"
setg   = generic1 "setg"
setge  = generic1 "setge"
setl   = generic1 "setl"
setle  = generic1 "setle"
setna  = generic1 "setna"
setnae = generic1 "setnae"
setnb  = generic1 "setnb"
setnbe = generic1 "setnbe"
setnc  = generic1 "setnc"
setne  = generic1 "setne"
setng  = generic1 "setng"
setnge = generic1 "setnge"
setnl  = generic1 "setnl"
setnle = generic1 "setnle"
setno  = generic1 "setno"
setnp  = generic1 "setnp"
setns  = generic1 "setns"
setnz  = generic1 "setnz"
seto   = generic1 "seto"
setp   = generic1 "setp"
setpe  = generic1 "setpe"
setpo  = generic1 "setpo"
sets   = generic1 "sets"
setz   = generic1 "setz"

-- Generic instruction taking two arguments.
generic2 :: (Arg a, Arg b) => String -> a -> b -> Asm ()
generic2 op x y = raw (op ++ " " ++ showArg x ++ ", " ++ showArg y ++ ";")

add      :: (Arg a, Arg b) => a -> b -> Asm ()
sub      :: (Arg a, Arg b) => a -> b -> Asm ()
imul     :: (Arg a, Arg b) => a -> b -> Asm ()
or       :: (Arg a, Arg b) => a -> b -> Asm ()
and      :: (Arg a, Arg b) => a -> b -> Asm ()
mov      :: (Arg a, Arg b) => a -> b -> Asm ()
movsx    :: (Arg a, Arg b) => a -> b -> Asm ()
movzx    :: (Arg a, Arg b) => a -> b -> Asm ()
movByte  :: (Arg a, Arg b) => a -> b -> Asm ()
movWord  :: (Arg a, Arg b) => a -> b -> Asm ()
movDword :: (Arg a, Arg b) => a -> b -> Asm ()
cmp      :: (Arg a, Arg b) => a -> b -> Asm ()
shr      :: (Arg a, Arg b) => a -> b -> Asm ()
shl      :: (Arg a, Arg b) => a -> b -> Asm ()

add      = generic2 "add"
sub      = generic2 "sub"
imul     = generic2 "imul"
or       = generic2 "or"
and      = generic2 "and"
mov      = generic2 "mov"
movsx    = generic2 "movsx"
movzx    = generic2 "movzx"
movByte  = generic2 "mov byte"
movWord  = generic2 "mov word"
movDword = generic2 "mov dword"
cmp      = generic2 "cmp"
shr      = generic2 "shr"
shl      = generic2 "shl"
