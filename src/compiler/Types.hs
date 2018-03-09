module Types
  ( getType
  ) where

import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import Data.Either
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Utils
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
getExpressionType wp s (Expression _ (NewExpression name arguments))
  = if foundConstructor
    then Right (Type (NamedType name) False)
    else Left "Could not find a matching constructor"
  where foundConstructor = and $ map (isRight . getExpressionType wp s) arguments
-- TODO: the previous just checks if the arguments are not error types.
-- TODO: we also need to lookup the constructor and overload

-- JLS 15.10: Array Creation Expressions
getExpressionType wp s (Expression _ (NewArrayExpression t sizeExpr)) = do
  sizeType <- getExpressionType wp s sizeExpr
  if isNumber sizeType
  then Left "Array size must be numeric type"
  else return t

-- TODO: some more expression remain
getExpressionType _ _ _ = Right Void


---------- Helper Functions ----------

isName :: InnerType -> Bool
isName (NamedType _) = True
isName _ = False

isNumber :: Type -> Bool
isNumber (Type Byte False)  = True
isNumber (Type Int False)   = True
isNumber (Type Short False) = True
isNumber _                  = False

isString :: Type -> Bool
isString t = typeSignature t == "java.lang.String"
