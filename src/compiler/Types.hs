module Types
  ( getType
  , getExpressionType
  , foldEither
  ) where

import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import Data.Either
import Control.Monad
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Utils
import JoosCompiler.TreeUtils

-- Collapse all the error messages into one error message.
foldEither :: [Either a b] -> Either a [b]
foldEither []           = Right []
foldEither (Left x:xs)  = Left x
foldEither (Right x:xs) = either Left (\xs -> Right (x:xs)) (foldEither xs)

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
  if isNumber sizeType
    then return t
    else Left ("Array size must be numeric type " ++ show e)

-- JLS 15.11: Field Access Expressions
getExpressionType wp s e@(Expression _ (FieldAccess primary name)) = do
  classType <- getExpressionType wp s primary
  if not $ isName classType -- TODO: arrays have a length field
    then Left ("Can only access fields of reference types " ++ show e)
    else do
      return (Type Int False) -- TODO: lookup

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
      if not $ isNumber sizeExpr
        then Left ("Can only perform array access with numbers " ++ show e)
        else return (toScalar arrayType)

-- JLS 15.15.4: Unary Minus Operator (-)
getExpressionType wp s e@(Expression _ (UnaryOperation Negate expr)) = do
  exprType <- getExpressionType wp s expr
  if not $ isNumber exprType
    then Left ("Can only negate numeric types " ++ show e)
    else return (Type Int False) -- promotion

-- JLS 15.15.6: Logical Complement Operator (!)
getExpressionType wp s e@(Expression _ (UnaryOperation Not expr)) = do
  exprType <- getExpressionType wp s expr
  if not $ isBoolean exprType
    then Left ("Can only not boolean types" ++ show e)
    else return exprType

-- JLS 15.16: Cast Expressions
-- TODO

-- JLS 15.18.1: String Concatenation Operator (+)
-- JLS 15.18.2: Additive Operators (+) for Numeric Types
-- The + operator is done separately from the other binary operators because strings.
getExpressionType wp s e@(Expression _ (BinaryOperation Add expr1 expr2)) = do
  expr1Type <- getExpressionType wp s expr1
  expr2Type <- getExpressionType wp s expr2
  if isString expr1Type || isString expr2Type
    then return (Type (NamedType ["java", "lang", "String"]) False)
    else if isNumber expr1Type && isNumber expr2Type
      then return (Type Int False) -- TODO: promotions
      else Left ("Bad addition types " ++ show e)

-- JLS 15.18.2: Additive Operators (-) for Numeric Types
-- JLS 15.17: Multiplicative Operators (*, /, %)
-- JLS 15.20: Relational Operators (<, >, <=, >=)
-- JLS 15.21: Equality Operators (==, !=)
-- JLS 15.22.2: Boolean Logical Operators &, ^, and |
-- JLS 15.23: Conditional-And Operator (&&)
-- JLS 15.24: Conditional-Or Operator (||)
-- JLS 15.26: Assignment Operators (=)
getExpressionType wp s e@(Expression _ (BinaryOperation op expr1 expr2)) = do
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

-- TODO: some more expression remain
getExpressionType _ _ _ = Right Void


---------- Helper Functions ----------

toScalar :: Type -> Type
toScalar (Type x _) = Type x False

isName :: Type -> Bool
isName (Type (NamedType _) False) = True
isName _                          = False

isNumber :: Type -> Bool
isNumber (Type Char False)  = True
isNumber (Type Byte False)  = True
isNumber (Type Int False)   = True
isNumber (Type Short False) = True
isNumber _                  = False

isReference :: Type -> Bool
isReference Null                   = True
isReference (Type _ True)          = True
isReference (Type (NamedType _) _) = True
isReference _                      = False

isBoolean :: Type -> Bool
isBoolean (Type Boolean False) = True
isBoolean _                    = False

isString :: Type -> Bool
isString t = typeSignature t == "java.lang.String"

-- Create method signature suitable for lookup.
createLookupSignature :: String -> [Type] -> String
createLookupSignature name args =
  methodSignature $ Method Void [] name (map (\t -> Local t [] "" $ emptyType This) args) []
