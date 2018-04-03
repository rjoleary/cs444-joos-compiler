{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Codegen.CodeGenType
  ( codeGenType
  ) where

import Data.List
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Tree
import Control.Monad
import Flow
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Utils
import Codegen.X86
import Codegen.Mangling
import qualified Codegen.X86 as X86
import qualified Data.Map.Strict as Map

codeGenType :: WholeProgram -> TypeDeclaration -> Either String (Asm ())
codeGenType wp t = Right $ generateTypeDeclaration ctx t
  where ctx = CodeGenCtx {
      ctxProgram     = wp,
      ctxThis        = typeCanonicalName t,
      ctxLocals      = Map.empty,
      ctxFrame       = Map.empty,
      ctxFrameOffset = 0 }

-- For local types only.
type LocalEnvironment = Map.Map String Type
type LocalFrame = Map.Map String Int32

data CodeGenCtx = CodeGenCtx
  { ctxProgram     :: WholeProgram
  , ctxThis        :: Name
  , ctxLocals      :: LocalEnvironment
  , ctxFrame       :: LocalFrame
  , ctxFrameOffset :: Int32
  }


---------- Type Declaration ----------

generateTypeDeclaration :: CodeGenCtx -> TypeDeclaration -> Asm ()
generateTypeDeclaration ctx t@TypeDeclaration{isInterface=False} = do
  let wp = ctxProgram ctx
  let staticFields = filter isFieldStatic $ classFields t

  -- instanceof table
  -- This excludes objects because (x instanceof Object) is true at compile time.
  let instanceOfTable = nub $ tail $ flatten $ typeHierarchy wp t
  comment "Instanceof table"
  dd (I 0)
  dd . L . mangle $ t
  mapM_ (\l -> (extern l) >> (dd . L . mangle $ l)) instanceOfTable
  space

  -- Vtable
  comment "TODO: vtable"
  global t
  label t
  space

  -- Uninitialized static fields
  comment (show (length staticFields) ++ " static fields")
  mapM_ (\field -> do
    comment (variableName field)
    label field
    dd (I 0)
    ) staticFields
  space

  -- Init function for initializing static variables.
  comment "Init function"
  global (Init t)
  label (Init t)
  -- Initialized in the order defined
  indent $ mapM_ (\field -> do
    comment (variableName field)
    generateExpression' ctx (variableValue field)
    mov Ebx (L (mangle field))
    mov (Addr Ebx) Eax
    ) staticFields
  space

  -- Constructors
  comment "Constructors"
  mapM_ (\m -> comment "Constructor" >> generateMethod' ctx m >> space) (constructors t)

  -- Methods
  comment "Methods"
  mapM_ (\m -> generateMethod' ctx m >> space) (methods t)

-- Interface have no code.
generateTypeDeclaration ctx t@TypeDeclaration{isInterface=True} = return ()


---------- Method ----------

-- This gets placed between each recursive call to generateMethod to indent
-- the block and add extra debug information.
generateMethod' :: CodeGenCtx -> Method -> Asm ()
generateMethod' ctx x = indent $ do
  comment (show x)
  generateMethod ctx x

generateMethod :: CodeGenCtx -> Method -> Asm ()
generateMethod ctx m
  -- Abstract methods
  | isMethodAbstract m = do
    return ()

  -- Native methods
  | isMethodNative m = do
    global m
    label m

    -- Move the first (and only) argument into eax.
    mov Eax (AddrOffset Esp 16)

    -- Jump to the native function as if it was the original callee.
    let nativeLabel = "NATIVE" ++ intercalate "." (methodCanonicalName m)
    extern nativeLabel
    jmp (L nativeLabel)

  -- constructor 
  | isConstructor m = do
    global m
    label m
    generateConstructor ctx m 
    generateStatement ctx (methodStatement m)
    mov Esp Ebp
    pop Ebp
    ret


  -- Regular static and non-static methods
  | otherwise = do
    global m
    label m
    push Ebp
    mov Ebp Esp

    -- The caller already pushed these arguments onto the stack.
    -- Type checking has already ensured static methods do not use "this", so
    -- the final parameter will simply be ignored for static methods.
    let thisType = Type (NamedType (ctxThis ctx)) False
    let paramNames = reverse $ "this":map variableName (methodParameters m)
    let paramTypes = reverse $ thisType:map variableType (methodParameters m)
    let newCtx = ctx {
      ctxLocals      = Map.fromList (zip paramNames paramTypes),
      -- The first argument is 16 to skip 4 registers: ebx, ebi, esi, link, ebp
      ctxFrame       = Map.fromList (zip paramNames [20,24..]),
      ctxFrameOffset = 0 }

    generateStatement newCtx (methodStatement m)
    mov Esp Ebp
    pop Ebp
    ret

---------- Statements ----------

-- This gets placed between each recursive call to generateStatement to indent
-- the block and add extra debug information.
generateStatement' :: CodeGenCtx -> Statement -> Asm ()
generateStatement' ctx x = indent $ do
  comment (show x)
  generateStatement ctx x

generateStatement :: CodeGenCtx -> Statement -> Asm ()

generateStatement ctx x@BlockStatement{} = do
  comment "block"
  generateStatement' ctx (statementBlock x)
  comment "next statement"
  generateStatement' ctx (nextStatement x)

generateStatement ctx x@ExpressionStatement{} = do
  comment "expression"
  generateExpression' ctx (statementExpression x)
  comment "next statement"
  generateStatement' ctx (nextStatement x)

generateStatement ctx x@IfStatement{} = do
  endLabel <- uniqueLabel
  elseLabel <- uniqueLabel
  t1 <- generateExpression' ctx (ifPredicate x)
  cmp Eax (I 0)
  je (L elseLabel)
  comment "then"
  generateStatement' ctx (ifThenStatement x)
  jmp (L endLabel)
  comment "else"
  label elseLabel
  generateStatement' ctx (ifElseStatement x)
  label endLabel
  comment "next statement"
  generateStatement' ctx (nextStatement x)

generateStatement ctx ReturnStatement{returnExpression=Just x} = do
  comment "return"
  generateExpression' ctx x
  mov Esp Ebp
  pop Ebp
  ret
  -- No next statement

generateStatement ctx ReturnStatement{returnExpression=Nothing} = do
  comment "return void"
  mov Esp Ebp
  pop Ebp
  ret
  -- No next statement

generateStatement ctx x@LocalStatement{} = do
  let var = localVariable x
  let varName = variableName var
  comment "Local declaration"
  t <- generateExpression' ctx (variableValue var)

  comment ("push " ++ varName ++ " to stack")
  push Eax

  let newCtx = ctx {
    ctxLocals      = Map.insert varName t (ctxLocals ctx),
    ctxFrame       = Map.insert varName (ctxFrameOffset ctx - 4) (ctxFrame ctx),
    ctxFrameOffset = ctxFrameOffset ctx - 4 }

  comment "next statement"
  generateStatement' newCtx (nextStatement x)

  comment ("remove " ++ varName ++ " from stack")
  add Esp (I 4)

generateStatement ctx x@LoopStatement{} = do
  startLabel <- uniqueLabel
  endLabel <- uniqueLabel
  label startLabel
  comment "loop condition"
  t1 <- generateExpression' ctx (loopPredicate x)
  cmp Eax (I 0)
  je (L endLabel)
  comment "loop body"
  generateStatement' ctx (loopStatement x)
  jmp (L startLabel)
  label endLabel
  comment "next statement"
  generateStatement' ctx (nextStatement x)

generateStatement ctx x@EmptyStatement{} = do
  -- No code
  return ()

generateStatement ctx x@TerminalStatement{} = do
  -- No code
  return ()


---------- Expressions ----------

-- This gets placed between each recursive call to generateExpression to indent
-- the block and add extra debug information.
generateExpression' :: CodeGenCtx -> Expression -> Asm Type
generateExpression' ctx e = indent $ do
  comment (show e)
  generateExpression ctx e

generateExpression :: CodeGenCtx -> Expression -> Asm Type

-- Add is a special binary operator because it is overloaded for strings.
generateExpression ctx (BinaryOperation Add x y) = do
  t1 <- generateExpression' ctx x
  push Eax
  t2 <- generateExpression' ctx y
  mov Ebx Eax
  pop Eax
  add Eax Ebx
  return (Type Int False) -- TODO: strings

-- And is special because short circuiting.
generateExpression ctx (BinaryOperation And x y) = do
  t1 <- generateExpression' ctx x
  cmp Eax (I 1)
  l <- uniqueLabel
  jne (L l)
  t2 <- generateExpression' ctx y
  label l
  return (Type Boolean False)

-- Or is special because short circuiting.
generateExpression ctx (BinaryOperation Or x y) = do
  t1 <- generateExpression' ctx x
  cmp Eax (I 1)
  l <- uniqueLabel
  je (L l)
  t2 <- generateExpression' ctx y
  label l
  return (Type Boolean False)

-- Assign is very special.
generateExpression ctx (BinaryOperation Assign x y) = do
  t1 <- generateLValue' ctx x
  push Eax
  t2 <- generateExpression' ctx y
  pop Ebx
  -- TODO: casting
  mov (Addr Ebx) Eax
  return t1

-- The rest of the binary operators are fairly generic.
generateExpression ctx (BinaryOperation op x y) = do
  t1 <- generateExpression' ctx x
  push Eax
  t2 <- generateExpression' ctx y
  mov Ebx Eax
  pop Eax
  binaryOperatorAsm op
  where
    binaryOperatorAsm :: BinaryOperator -> Asm Type
    binaryOperatorAsm Multiply     = imul Eax Ebx >> return (Type Int False)
    binaryOperatorAsm Divide       = idiv Ebx >> return (Type Int False)
    binaryOperatorAsm Modulus      = idiv Ebx >> mov Edx Eax >> return (Type Int False)
    binaryOperatorAsm Subtract     = sub Eax Ebx >> return (Type Int False)
    binaryOperatorAsm Less         = cmp Eax Ebx >> setl Al >> return (Type Boolean False)
    binaryOperatorAsm Greater      = cmp Eax Ebx >> setg Al >> return (Type Boolean False)
    binaryOperatorAsm LessEqual    = cmp Eax Ebx >> setle Al >> return (Type Boolean False)
    binaryOperatorAsm GreaterEqual = cmp Eax Ebx >> setge Al >> return (Type Boolean False)
    binaryOperatorAsm Equality     = cmp Eax Ebx >> sete Al >> return (Type Boolean False)
    binaryOperatorAsm Inequality   = cmp Eax Ebx >> setne Al >> return (Type Boolean False)
    binaryOperatorAsm LazyAnd      = X86.and Eax Ebx >> return (Type Boolean False)
    binaryOperatorAsm LazyOr       = X86.or Eax Ebx >> return (Type Boolean False)
    binaryOperatorAsm x            = error ("Codegen does not support binary operator '" ++ show x ++ "'")

generateExpression ctx (UnaryOperation Negate x) = do
  t <- generateExpression' ctx x
  mov Ebx Eax
  mov Eax (I 0)
  sub Eax Ebx
  return (Type Int False)

generateExpression ctx (UnaryOperation Not x) = do
  t <- generateExpression' ctx x
  mov Ebx Eax
  mov Eax (I 1)
  sub Eax Ebx
  return (Type Boolean False)

generateExpression ctx (LiteralExpression (IntegerLiteral x)) = do
  mov Eax (I $ fromInteger x) -- TODO: double check extremities
  return (Type Int False)

generateExpression ctx (LiteralExpression (BooleanLiteral x)) = do
  mov Eax (I (if x == True then 1 else 0))
  return (Type Boolean False)

generateExpression ctx (LiteralExpression (CharacterLiteral x)) = do
  if isAlphaNum x
    then raw ("mov eax, " ++ show x ++ ";") -- Pretty print characters
    else mov Eax (I $ fromInteger $ toInteger $ ord x)
  return (Type Char False)

generateExpression ctx (LiteralExpression (StringLiteral x)) = do
  strStart <- uniqueLabel
  strEnd <- uniqueLabel
  comment ("StringLiteral " ++ show x)
  jmp (L strEnd) -- Jump over the string
  label strStart
  mapM_ (\c ->
    if isAlphaNum c
      then raw ("dd " ++ show c ++ ";") -- Pretty print characters
      else dd (I $ fromInteger $ toInteger $ ord c)
    ) x
  label strEnd
  mov Eax (L strStart)
  mov Ebx (I $ fromIntegral $ length x)
  extern "allocStrLiteral"
  call (L "allocStrLiteral")
  return (Type (NamedType ["java", "lang", "String"]) False)

generateExpression ctx (LiteralExpression NullLiteral) = do
  mov Eax (I 0)
  return Null

-- "this" is treated as a special kind of local argument.
generateExpression ctx This = generateExpression ctx (LocalAccess "this")

generateExpression ctx ExpressionName{} = do
  comment "TODO ExpressionName"
  --error "ExpressionName should never be present into CodeGen"
  return Void

generateExpression ctx (NewExpression n e) = do
  mov Eax (I $fromIntegral v)
  mov Ebx Eax -- ebx contains the number of fields
  add Eax (I 1)
  push Ebx
  shl Eax (I 2)
  extern "__malloc"
  call (L "__malloc")
  -- This special extern prevents externing something in the current file.
  let extern = externIfRequired (getTypeInProgram (ctxProgram ctx) (ctxThis ctx))
    in extern addr
  movDword (Addr Eax) (L addr)
  push Eax
  -- Eax contains the start address of the object.
  -- initialize fields
  add Eax (I 4)
  extern "memclear"
  call (L "memclear")

  -- call constructor
  t <- mapM_ (\(idx, arg) -> do
    comment ("Argument number " ++ show idx)
    t <- generateExpression' ctx arg
    push Eax
    ) (zip [1..] e)
  push Ebx
  push Edi
  push Esi

  -- get constructor name
  types <- mapM (generateExpression' ctx) e
  let maybeTp = resolveTypeInProgram (ctxProgram ctx) n
  let tp = fromMaybe (error "Could not solve type") maybeTp
  let maybeCtor = findOverload "" types (constructors tp)
  let ctorName = fromMaybe (error "Does not contain a constructure") maybeCtor
  let ctorMangleName = mangle ctorName
  
  extern ctorMangleName
  mov Eax (L ctorMangleName)
  call Eax
  pop Esi
  pop Edi
  pop Ebx
  add Esp (I $ fromIntegral (length e * 4))
  pop Eax
 -- call Eax
 -- add Esp (I 4)
  -- pop Eax
  -- pop Eax  
  -- pop Ebx
  -- push Eax
  -- add Eax (I 4)
  -- push Eax
  -- t <- mapM_ (initializeObjectField ctx wp n) fields
  -- pop Eax
  return (methodReturn ctorName)
  where
    addr = mangle td
    td = fromMaybe (error "Could not resolve type") maybeTd
    maybeTd = resolveTypeInProgram wp n
    v = length fields
    fields = directAndIndirectDynamicFields wp n
    wp = ctxProgram ctx

generateExpression ctx (NewArrayExpression t e) = do
  t <- generateExpression' ctx e
--  add Eax (I 1)
  mov Ebx Eax
  add Eax (I 2)
  push Ebx
  -- malloc allocate bytes? 
  shl Eax (I 2)
  extern "__malloc"
  call (L "__malloc")
  pop Ebx
  -- This special extern prevents externing something in the current file.
  let extern = externIfRequired (getTypeInProgram (ctxProgram ctx) (ctxThis ctx))
    in case it of
      (NamedType name) -> extern td >> movDword (Addr Eax) (L (mangle td))
      otherwise        -> movDword (Addr Eax) (I 0)
  push Eax
  add Eax (I 4)
  movDword (Addr Eax) Ebx
  pop Eax
  push Eax
  extern "memclear"
  call (L "memclear")
  pop Eax
  return t
  where
    it = innerType t
    maybeTd = resolveTypeInProgram (ctxProgram ctx) (unNamedType it)
    td = fromMaybe (error "Could not resolve type") maybeTd
    obj = mangle td

generateExpression ctx (CastExpression t e) = do
  generateExpression' ctx e -- TODO: is instanceof
  -- TODO: (x instanceof Object) is true at compile time
  return t

generateExpression ctx (StaticMethodInvocation n s args) = do
  t <- mapM_ (\(idx, arg) -> do
    comment ("Argument number " ++ show idx)
    t <- generateExpression' ctx arg
    push Eax
    ) (zip [1..] args)
  push Ebx
  push Edi
  push Esi
  -- This special extern prevents externing something in the current file.
  let extern = externIfRequired (getTypeInProgram (ctxProgram ctx) (ctxThis ctx))
    in extern na
  mov Eax (L (mangle na))
  call Eax
  pop Esi
  pop Edi
  pop Ebx
  add Esp (I $ fromIntegral (length args * 4))
  return (methodReturn $ na)
    where
      argTypes = [Type Int False]
      methods = resolveStaticMethodInProgram (ctxProgram ctx) n s argTypes
      na = case methods of
        (m:_) -> m
        []    -> error $ "Expected to find a method: " ++ (showName (n ++ [s]))

generateExpression ctx (InstanceOfExpression e t) = do
  comment "TODO InstanceOfExpression"
  mov Eax (I 123)
  return Void


--generateExpression ctx (ArrayExpression expr exprIdx) = do
generateExpression ctx (ArrayExpression expr exprIdx) = do
  t <- generateLValue' ctx (ArrayExpression expr exprIdx)
  mov Eax (Addr Eax)
  return t

generateExpression ctx (AmbiguousFieldAccess _ _) = do
  comment "TODO AmbiguousFieldAccess -- make this an error"
  return Void

generateExpression ctx (DynamicMethodInvocation _ _ _) = do
  comment "TODO DynamicMethodInvocation"
  mov Eax (I 123)
  return Void

generateExpression ctx (DynamicFieldAccess _ _) = do
  comment "TODO DynamicFieldAccess"
  mov Eax (I 123)
  return Void

generateExpression ctx (ArrayLengthAccess _) = do
  comment "TODO ArrayLengthAccess"
  mov Eax (I 123)
  return Void

generateExpression ctx (StaticFieldAccess _) = do
  comment "TODO StaticFieldAccess"
  mov Eax (I 123)
  return Void

generateExpression ctx (LocalAccess n) = do
  mov Eax $ AddrOffset Ebp (mapLookupWith (ctxFrame ctx))
  return $ mapLookupWith (ctxLocals ctx)
  where mapLookupWith m = fromMaybe (error $ "Could not find " ++ n) $ Map.lookup n m

generateExpression ctx (ClassAccess _) = error "ClassAccess should not reach code generation"

---------- LValues ----------

-- This gets placed between each recursive call to generateLValue to indent
-- the block and add extra debug information.
generateLValue' :: CodeGenCtx -> Expression -> Asm Type
generateLValue' ctx e = indent $ do
  comment (show e)
  generateLValue ctx e

generateLValue :: CodeGenCtx -> Expression -> Asm Type

generateLValue ctx (LocalAccess n) = do
  mov Eax Ebp
  add Eax (I $ (fromMaybe (error "Could not find local") $ Map.lookup n (ctxFrame ctx)))
  return $ fromMaybe (error "Could not find local") $ Map.lookup n (ctxLocals ctx)

-- TODO: other lvalues
generateLValue ctx (ArrayExpression expr exprIdx) = do
  t <- generateExpression' ctx expr
  push Eax
  p <- generateExpression' ctx exprIdx
  mov Ebx Eax
  pop Eax
  extern "nullcheck"
  call (L "nullcheck")
  cmp Ebx (I 0)
  extern "__exception"
  jl (L "__exception")
  push Eax
  add Eax (I 4)
  cmp (Addr Eax) Ebx
  extern "__exception"
  jle (L "__exception")
  pop Eax
  add Ebx (I 2)
  shl Ebx (I 2)
  add Eax Ebx
  return t

generateLValue _ _ = do
-- comment "TODO"
  mov Eax (I 123)
  return Void

generateConstructor :: CodeGenCtx -> Method -> Asm()
generateConstructor ctx m 
  | isNoSuperObject ctx m = do
    push Ebp
    mov Esp Ebp

  | otherwise = do
    push Ebp
    mov Esp Ebp
    push Ebp
    mov Esp Ebp
    -- call super first
      --push this into stack

    let thisLabel = getTypeInProgram (ctxProgram ctx) (ctxThis ctx)
    mov Eax (L thisLabel)
    push Eax

      -- save registers
    push Ebx
    push Edi
    push Esi
    let superName = super(getTypeInProgram (ctxProgram ctx) (ctxThis ctx))
    let superConstructorLabel = "Class$" ++ intercalate "$" superName 
    extern superConstructorLabel
    call (L superConstructorLabel)
    pop Esi
    pop Edi
    pop Ebx
    ret

initializeObjectField :: CodeGenCtx -> WholeProgram -> Name -> Variable -> Asm Type
initializeObjectField ctx wp n var = do
  ge <- generateExpression' ctx (variableValue var)
  mov Ebx Eax
  pop Eax
  push Eax
  add Eax (I offset)
  mov (Addr Eax) Ebx
  return (variableType var)
  where
    offset = getDynamicFieldOffset wp n var

getDynamicFieldOffset :: WholeProgram -> Name -> Variable -> Int32
getDynamicFieldOffset wp n var = fromInteger $ toInteger offset
  where
    offset = index * 4
    index = fromMaybe (error "Object doesn't have this field") maybeIndex
    maybeIndex = elemIndex var vars
    vars = directAndIndirectDynamicFields wp n

isNoSuperObject :: CodeGenCtx -> Method -> Bool
isNoSuperObject ctx x = superLabel == "java$lang$Object"
  where
    superLabel = intercalate "$" superName
    superName = (super(getTypeInProgram (ctxProgram ctx) (ctxThis ctx)))
