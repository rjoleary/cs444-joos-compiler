{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Codegen.CodeGenType
  ( codeGenType
  ) where

import Data.Char
import Data.Int
import Data.Maybe
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Utils
import JoosCompiler.Ast.Visitor.Analysis
import Codegen.X86
import Codegen.Mangling
import qualified Codegen.X86 as X86
import qualified Data.Map.Strict as Map

codeGenType :: WholeProgram -> TypeDeclaration -> Either String (Asm ())
codeGenType wp t = analyze' ctx t
  where ctx = CodeGenCtx {
      ctxProgram     = wp
    , ctxLocals      = Map.empty
    , ctxFrame       = Map.empty
    , ctxFrameOffset = 0
    }

-- For local types only.
type LocalEnvironment = Map.Map String Type
type LocalFrame = Map.Map String Int32

data CodeGenCtx = CodeGenCtx
  { ctxProgram     :: WholeProgram
  , ctxLocals      :: LocalEnvironment
  , ctxFrame       :: LocalFrame
  , ctxFrameOffset :: Int32
  }

instance Analysis CodeGenCtx (Asm ()) where
  analyze ctx (AstTypeDeclaration t@TypeDeclaration{isInterface=False}) = Right $ do
    let wp = ctxProgram ctx
    global t
    label t
    space

    -- instanceof table
    -- This excludes objects because (x instanceof Object) is true at compile time.
    let instanceOfTable = filter (\x -> typeCanonicalName x /= ["java", "lang", "Object"]) $ map (fromJust . resolveTypeInProgram wp) (super t : interfaces t)
    comment ("instanceof table of size " ++ show (length instanceOfTable + 1))
    mapM_ (\l -> (extern l) >>  (dd . L . mangle $ l)) instanceOfTable
    dd (I 0)
    space

    -- Vtable
    comment "TODO: vtable"
    space

    -- Uninitialized static fields
    comment (show (length staticFields) ++ " static fields")
    mapM_ (\field -> do
      comment (variableName field)
      label field
      dd (I 0)
      ) staticFields
    space

    -- Init function
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
    comment "TODO: constructors"
    space

    -- Methods
    comment "Methods"
    mapM_ (\m -> generateMethod' ctx m >> space) (methods t)

    where staticFields = filter isFieldStatic $ classFields t

  analyze ctx (AstTypeDeclaration t@TypeDeclaration{isInterface=True}) = Right $ do
    label t
    dd (I 0x7654321) -- TODO

  -- Everything else propagates.
  analyze ctx x = propagateAnalyze ctx x


---------- Method ----------

generateMethod' :: CodeGenCtx -> Method -> Asm ()
generateMethod' ctx x = indent $ do
  comment (show x)
  generateMethod ctx x

generateMethod :: CodeGenCtx -> Method -> Asm ()
generateMethod ctx m = do
  global m
  label m
  -- TODO: arguments
  generateStatement ctx (methodStatement m)


---------- Statements ----------

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
  ret
  -- No next statement

generateStatement ctx ReturnStatement{returnExpression=Nothing} = do
  comment "return void"
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
    ctxFrame       = Map.insert varName (ctxFrameOffset ctx + 4) (ctxFrame ctx),
    ctxFrameOffset = ctxFrameOffset ctx + 4 }

  comment "next statement"
  generateStatement' newCtx (nextStatement x)

  comment ("remove " ++ varName ++ " from stack")
  sub Ebp (I 4)

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
  -- TODO
  t2 <- generateExpression' ctx y
  t1 <- generateExpression' ctx x
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

generateExpression ctx (LiteralExpression NullLiteral) = do
  mov Eax (I 0)
  return Null

generateExpression ctx (CastExpression t e) = do
  generateExpression' ctx e -- TODO: is instanceof
  -- TODO: (x instanceof Object) is true at compile time
  return t

-- TODO: other expressions
generateExpression _ _ = do
  comment "TODO"
  mov Eax (I 123)
  return Void
