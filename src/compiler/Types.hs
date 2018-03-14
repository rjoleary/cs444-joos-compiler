module Types
  ( getType
  , getExpressionType
  , isName
  , isNumeric
  , isReference
  , isPrimitive
  , isBoolean
  , isString
  ) where

import Data.Char
import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import Data.Either
import Data.Maybe
import Control.Monad
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Utils
import JoosCompiler.Error
import JoosCompiler.TreeUtils

getType :: WholeProgram -> Scope -> Expression -> Type
getType _ _ _ = Type Int False

getExpressionType :: WholeProgram -> Scope -> Expression -> Either String Type

-- JLS 15.8.1: Lexical Literals
getExpressionType _ _ (Expression _ (LiteralExpression t))
  = Right (literalType t)

-- JLS 15.8.3: this
getExpressionType _ _ (Expression _ (This))
  = Right (Type (NamedType ["java", "lang", "String"]) False) -- TODO: resolve to this class

-- JLS 15.9: Class Instance Creation Expressions
getExpressionType wp s e@(Expression _ (NewExpression name arguments))
  = if foundConstructor
    then Right (Type (NamedType name) False)
    else Left ("Could not find a matching constructor" ++ show e)
  where foundConstructor = and $ map (isRight . getExpressionType wp s) arguments
-- TODO: the previous just checks if the arguments are not error types.
-- TODO: we also need to lookup the constructor and overload

-- JLS 15.10: Array Creation Expressions
getExpressionType wp s e@(Expression _ (NewArrayExpression t sizeExpr)) = do
  sizeType <- getExpressionType wp s sizeExpr
  if isNumeric sizeType
    then return t
    else Left ("Array size must be numeric type " ++ show e)

-- JLS 15.11: Field Access Expressions
getExpressionType wp s e@(Expression _ (FieldAccess primary name)) = do
  classType <- getExpressionType wp s primary
  if not $ isName classType -- TODO: arrays have a length field
    then Left ("Can only access fields of reference types " ++ show e)
    else return $ Type Int False
    --else return $ resolveToType wp s name -- TODO: wrong resolve

-- JLS 15.12: Method Invocation Expressions
getExpressionType wp s e@(Expression _ (MethodInvocation expr name argExprs)) = do
  exprType <- getExpressionType wp s expr
  when (not $ isReference exprType)
    (Left $ "Method may only be invoked on reference types " ++ show e)
  argTypes <- foldEither $ map (getExpressionType wp s) argExprs
  let lookupSignature = createLookupSignature name argTypes
  return (Type Int False) -- TODO: lookup

-- JLS 15.13: Array Access Expressions
getExpressionType wp s e@(Expression _ (ArrayExpression arrayExpr sizeExpr)) = do
  arrayType <- getExpressionType wp s arrayExpr
  sizeExpr <- getExpressionType wp s sizeExpr
  if not $ isArray arrayType
    then Left ("Can only perform array access on an array " ++ show e)
    else do
      if not $ isNumeric sizeExpr
        then Left ("Can only perform array access with numbers " ++ show e)
        else return (toScalar arrayType)

-- JLS 15.15.4: Unary Minus Operator (-)
getExpressionType wp s e@(Expression _ (UnaryOperation Negate expr)) = do
  exprType <- getExpressionType wp s expr
  if not $ isNumeric exprType
    then Left ("Can only negate numeric types " ++ show e)
    else return (Type Int False) -- promotion

-- JLS 15.15.6: Logical Complement Operator (!)
getExpressionType wp s e@(Expression _ (UnaryOperation Not expr)) = do
  exprType <- getExpressionType wp s expr
  if not $ isBoolean exprType
    then Left ("Can only not boolean types" ++ show e)
    else return exprType

-- JLS 15.16: Cast Expressions
getExpressionType wp s e@(Expression _ (CastExpression t expr)) = do
  exprType <- getExpressionType wp s expr
  return $ t -- TODO: there are more rules

-- Binary Operations
getExpressionType wp s e@(Expression _ (BinaryOperation op expr1 expr2))

  -- JLS 15.17: Multiplicative Operators (*, /, %)
  | op `elem` [Multiply, Divide, Modulus] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if isNumeric expr1Type && isNumeric expr2Type
      then return (Type Int False)
      else Left ("Bad multiplicative types" ++ show e)

  -- JLS 15.18.1: String Concatenation Operator (+)
  -- JLS 15.18.2: Additive Operators (+) for Numeric Types
  | op `elem` [Add] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if isString expr1Type || isString expr2Type
      then return (Type (NamedType ["java", "lang", "String"]) False)
      else if isNumeric expr1Type && isNumeric expr2Type
        then return (Type Int False) -- TODO: promotions
        else Left ("Bad addition types " ++ show e)

  -- JLS 15.18.2: Additive Operators (-) for Numeric Types
  | op `elem` [Subtract] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if isNumeric expr1Type && isNumeric expr2Type
      then return (Type Int False)
      else Left ("Bad Subtract types " ++ show e)

  -- JLS 15.20: Relational Operators (<, >, <=, >=)
  | op `elem` [Less, Greater, LessEqual, GreaterEqual] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if (isNumeric expr1Type && isNumeric expr2Type)
      then return (Type Boolean False) -- TODO: more checks are required
      else Left ("Bad binary operator " ++ show e)

  -- JLS 15.21: Equality Operators (==, !=)
  | op `elem` [Equality, Inequality] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if (expr1Type == expr2Type)
       then return (Type Boolean False)
       else Left ("Bad Equality types " ++ show e)

  -- JLS 15.22.2: Boolean Logical Operators &, ^, and |
  -- JLS 15.23: Conditional-And Operator (&&)
  -- JLS 15.24: Conditional-Or Operator (||)
  | op `elem` [LazyAnd, LazyOr, And, Or] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if (isBoolean expr1Type && isBoolean expr2Type)
      then return (Type Boolean False) -- TODO: more checks are required
      else Left ("Bad binary operator " ++ show e)

  -- JLS 15.26: Assignment Operators (=)
  | op `elem` [Assign] = do
    expr1Type <- getExpressionType wp s expr1
    expr2Type <- getExpressionType wp s expr2
    if expr1Type == expr2Type
      then return expr1Type -- TODO: more checks are required
      else Left ("Bad binary operator " ++ show e)

-- JLS 15.20.2: Type Comparison Operator instanceof
getExpressionType wp s e@(Expression _ (InstanceOfExpression expr t)) = do
  exprType <- getExpressionType wp s expr
  if (isReference exprType || exprType == Null) && (isReference t)
    then return (Type Boolean False)
    else Left ("Bad instanceof operator " ++ show e)

{-

getExpressionType wp s (Expression _ (ExpressionName name)) =
  return $ fromMaybe localType typeType
  where typeDecl = resolveTypeFromProgram wp name
        typeType = fmap (\x -> Type (NamedType [typeName x]) False) typeDecl
        localType = resolveToType wp s (showName name)

-}

getExpressionType wp s (Expression _ (ExpressionName name)) =
  return $ Type Int False -- TODO: this is wrong

getExpressionType _ _ _ = return $ Type Int False


---------- Helper Functions ----------

toScalar :: Type -> Type
toScalar (Type x _) = Type x False

isName :: Type -> Bool
isName (Type (NamedType _) False) = True
isName _                          = False

isNumeric :: Type -> Bool
isNumeric (Type Char False)  = True
isNumeric (Type Byte False)  = True
isNumeric (Type Int False)   = True
isNumeric (Type Short False) = True
isNumeric _                  = False

isReference :: Type -> Bool
isReference Null                   = True
isReference (Type _ True)          = True
isReference (Type (NamedType _) _) = True
isReference _                      = False

isPrimitive :: Type -> Bool
isPrimitive (Type Boolean False) = True
isPrimitive (Type Byte False)    = True
isPrimitive (Type Char False)    = True
isPrimitive (Type Int False)     = True
isPrimitive (Type Short False)   = True
isPrimitive _                    = False

isBoolean :: Type -> Bool
isBoolean (Type Boolean False) = True
isBoolean _                    = False

isString :: Type -> Bool
isString t = typeSignature t == "java.lang.String"

-- Create method signature suitable for lookup.
createLookupSignature :: String -> [Type] -> String
createLookupSignature name args =
  methodSignature $ Method Void [] name (map (\t -> Local t [] "" $ emptyType This) args) []

-- Fields and Locals are essentially the same and are combined here.
resolveAsLocal :: WholeProgram -> Scope -> String -> Local
resolveAsLocal wp s name = asLocal $ resolveInScope wp s [name]
  where
    asLocal (Left (Field a b c d)) = Local a b c d
    asLocal (Right x)              = x

resolveToType :: WholeProgram -> Scope -> String -> Type
resolveToType wp s name = localType $ resolveAsLocal wp s name
