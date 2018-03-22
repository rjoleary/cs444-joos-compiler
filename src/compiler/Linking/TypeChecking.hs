{-# LANGUAGE MultiParamTypeClasses #-}
module Linking.TypeChecking
  ( checkTypes
  ) where

import Control.Monad
import Data.Char
import Data.Either
import Data.Function
import Data.List
import Data.List.Unique
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Tree
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Transformers.Types
import JoosCompiler.Ast.Visitor.Analysis
import JoosCompiler.Ast.Utils
import JoosCompiler.Error
import JoosCompiler.TreeUtils

checkTypes :: AstNode -> Either String ()
checkTypes (Node x _) = analyze (TypeAnalysis Map.empty Nothing Nothing) x

-- For local types only.
type LocalEnvironment = Map.Map String Type

-- TODO: add global environment
data TypeAnalysis = TypeAnalysis
  { ctxLocalEnv :: LocalEnvironment
  , ctxThis     :: Maybe Name
  , ctxThisType :: Maybe TypeDeclaration } -- TODO: this last field is temporary

-- Analysis on classes/methods/statements. No Type is returned.
instance Analysis TypeAnalysis () where
  -- TODO: This skips type checking on standard libraries. However, it should pass.
  analyze ctx (AstCompilationUnit (CompilationUnit{cuPackage=("java":_)})) =
    return ()

  analyze ctx a@(AstTypeDeclaration x) =
    propagateAnalyze ctx{ ctxThis = Just [typeName x], ctxThisType = Just x } a

  analyze ctx a@(AstField v) = do
    exprType <- analyze' ctx (variableValue v)
    when (exprType /= variableType v)
      (Left $ "Field type doesn't match (got " ++ show exprType ++ ", expected " ++ show (variableType v) ++ ")")
    -- `this` is inaccessible in field declarations.
    propagateAnalyze ctx{ ctxThis = Nothing } a

  analyze ctx (AstStatement s@ExpressionStatement{nextStatement=n}) = do
    -- The type annotation is required here because the return is ignored.
    exprType <- analyze' ctx (statementExpression s) :: Either String Type
    analyze' ctx n

  analyze ctx (AstStatement s@LoopStatement{nextStatement=n}) = do
    predicateType <- analyze' ctx (loopPredicate s)
    when (not $ isBoolean $ predicateType)
      (Left "Loop predicate must be a boolean")
    analyze' ctx n

  analyze ctx (AstStatement s@IfStatement{nextStatement=n}) = do
    predicateType <- analyze' ctx (ifPredicate s)
    when (not $ isBoolean $ predicateType)
      (Left "If predicate must be a boolean")
    analyze' ctx n

  analyze ctx (AstStatement ReturnStatement{returnExpression=Just e, nextStatement=n}) = do
    returnType <- analyze' ctx e
    when (returnType == Void) (Left "Cannot return void")
    analyze' ctx n

  analyze ctx (AstStatement LocalStatement{localVariable=v, nextStatement=n}) = do
    exprType <- analyze' ctx (variableValue v)
    when (exprType /= variableType v)
      (Left $ "Local statement type doesn't match (got " ++ show exprType ++ ", expected " ++ show (variableType v) ++ ")")
    let newEnv = Map.insert (variableName v) (variableType v) (ctxLocalEnv ctx)
    analyze' ctx{ ctxLocalEnv = newEnv } n

  analyze ctx x = propagateAnalyze ctx x

-- Analysis on expressions. Type is returned.
instance Analysis TypeAnalysis Type where
  -- JLS 15.8.1: Lexical Literals
  analyze ctx (AstExpression (LiteralExpression t))
    = Right (literalType t)

  -- JLS 15.8.3: this
  analyze TypeAnalysis{ctxThis=Nothing} (AstExpression This)
    = Left ("Not a valid location for the this keyword")
  analyze TypeAnalysis{ctxThis=Just name} (AstExpression This)
    = Right (Type (NamedType name) False)

  -- JLS 15.9: Class Instance Creation Expressions
  analyze ctx (AstExpression e@(NewExpression name arguments))
    = if foundConstructor
      then Right (Type (NamedType name) False) -- TODO: qualify
      else Left ("Could not find a matching constructor" ++ show e)
    where foundConstructor = and $ map (isRight . analyze'') arguments
          analyze'' x = analyze' ctx x :: Either String Type
  -- TODO: the previous just checks if the arguments are not error types.
  -- TODO: we also need to lookup the constructor and overload

  -- JLS 15.10: Array Creation Expressions
  analyze ctx (AstExpression e@(NewArrayExpression t sizeExpr)) = do
    sizeType <- analyze' ctx sizeExpr
    if isNumeric sizeType
      then return (toArray t)
      else Left ("Array size must be numeric type " ++ show e)

  -- JLS 15.11: Field Access Expressions
  analyze ctx (AstExpression e@(FieldAccess primary name)) = do
    classType <- analyze' ctx primary
    if not $ isName classType -- TODO: arrays have a length field
      then Left ("Can only access fields of reference types " ++ show e)
      else fromMaybe (Left "Cannot find field") $ fmap Right $ lookup name [(variableName x, variableType x) | x <- classFields $ fromJust $ ctxThisType ctx]
      -- TODO: the above assumes the primary is of this type

  -- JLS 15.12: Method Invocation Expressions
  analyze ctx (AstExpression e@(MethodInvocation expr name argExprs)) = do
    exprType <- analyze' ctx expr
    when (not $ isReference exprType)
      (Left $ "Method may only be invoked on reference types " ++ show e)
    argTypes <- foldEither $ map (analyze' ctx) argExprs
    let lookupSignature = createLookupSignature name argTypes
    return (Type Int False) -- TODO: lookup

  -- JLS 15.13: Array Access Expressions
  analyze ctx (AstExpression e@(ArrayExpression arrayExpr sizeExpr)) = do
    arrayType <- analyze' ctx arrayExpr
    sizeExpr <- analyze' ctx sizeExpr
    if not $ isArray arrayType
      then Left ("Can only perform array access on an array " ++ show e)
      else do
        if not $ isNumeric sizeExpr
          then Left ("Can only perform array access with numbers " ++ show e)
          else return (toScalar arrayType)

  -- JLS 15.15.4: Unary Minus Operator (-)
  analyze ctx (AstExpression e@(UnaryOperation Negate expr)) = do
    exprType <- analyze' ctx expr
    if not $ isNumeric exprType
      then Left ("Can only negate numeric types " ++ show e)
      else return (Type Int False) -- promotion

  -- JLS 15.15.6: Logical Complement Operator (!)
  analyze ctx (AstExpression e@(UnaryOperation Not expr)) = do
    exprType <- analyze' ctx expr
    if not $ isBoolean exprType
      then Left ("Can only not boolean types" ++ show e)
      else return exprType

  -- JLS 15.16: Cast Expressions
  analyze ctx (AstExpression e@(CastExpression t expr)) = do
    exprType <- analyze' ctx expr :: Either String Type
    return $ t -- TODO: there are more rules

  -- Binary Operations
  analyze ctx (AstExpression e@(BinaryOperation op expr1 expr2))

    -- JLS 15.17: Multiplicative Operators (*, /, %)
    | op `elem` [Multiply, Divide, Modulus] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if isNumeric expr1Type && isNumeric expr2Type
        then return (Type Int False)
        else Left ("Bad multiplicative types" ++ show e)

    -- JLS 15.18.1: String Concatenation Operator (+)
    -- JLS 15.18.2: Additive Operators (+) for Numeric Types
    | op `elem` [Add] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if isString expr1Type || isString expr2Type
        then return (Type (NamedType ["java", "lang", "String"]) False)
        else if isNumeric expr1Type && isNumeric expr2Type
          then return (Type Int False) -- TODO: promotions
          else Left ("Bad additive types " ++ show e)

    -- JLS 15.18.2: Additive Operators (-) for Numeric Types
    | op `elem` [Subtract] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if isNumeric expr1Type && isNumeric expr2Type
        then return (Type Int False)
        else Left ("Bad additive types " ++ show e)

    -- JLS 15.20: Relational Operators (<, >, <=, >=)
    | op `elem` [Less, Greater, LessEqual, GreaterEqual] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if (isNumeric expr1Type && isNumeric expr2Type)
        then return (Type Boolean False) -- TODO: more checks are required
        else Left ("Bad relational types " ++ show e)

    -- JLS 15.21: Equality Operators (==, !=)
    | op `elem` [Equality, Inequality] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if ((expr1Type :: Type) == (expr2Type :: Type))
         then return (Type Boolean False)
         else Left ("Bad equality types " ++ show e)

    -- JLS 15.22.2: Boolean Logical Operators &, ^, and |
    -- JLS 15.23: Conditional-And Operator (&&)
    -- JLS 15.24: Conditional-Or Operator (||)
    | op `elem` [LazyAnd, LazyOr, And, Or] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if (isBoolean expr1Type && isBoolean expr2Type)
        then return (Type Boolean False) -- TODO: more checks are required
        else Left ("Bad conditional types " ++ show e)

    -- JLS 15.26: Assignment Operators (=)
    | op `elem` [Assign] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if expr1Type == expr2Type
        then return expr1Type -- TODO: more checks are required
        else Left ("Bad assignment operator " ++ show e)

  -- JLS 15.20.2: Type Comparison Operator instanceof
  analyze ctx (AstExpression e@(InstanceOfExpression expr t)) = do
    exprType <- analyze' ctx expr
    if (isReference exprType || exprType == Null) && (isReference t)
      then return (Type Boolean False)
      else Left ("Bad instanceof operator " ++ show e)

  -- TODO: disambiguation
  analyze ctx (AstExpression (ExpressionName name)) =
    if (isJust maybeLocalType)
    then (return $ fromJust maybeLocalType)
    else analyze' ctx (FieldAccess This (last name))
    -- TODO: global env
    where maybeLocalType = Map.lookup (last name) (ctxLocalEnv ctx) -- TODO: last is wrong

---------- Helper Functions ----------

toScalar :: Type -> Type
toScalar (Type x _) = Type x False

toArray :: Type -> Type
toArray (Type x _) = Type x True

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
  methodSignature $ Method Void [] name (map (\t -> Variable t [] "" This []) args) TerminalStatement []

leftOrRight :: Either a a -> a
leftOrRight (Left x) = x
leftOrRight (Right x) = x

resolveToType :: WholeProgram -> Scope -> String -> Type
resolveToType wp s name = variableType $ leftOrRight $ resolveInScope wp s [name]
