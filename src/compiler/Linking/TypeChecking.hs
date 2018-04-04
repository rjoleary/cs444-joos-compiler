{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Linking.TypeChecking
  ( checkTypes
  , canNumericNarrow
  , toScalar
  , isString
  , isReference
  , getTypeName
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
import JoosCompiler.Ast.ConstantExpression
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Transformers.Types
import JoosCompiler.Ast.Visitor.Analysis
import JoosCompiler.Ast.Utils
import JoosCompiler.Error
import JoosCompiler.TreeUtils

checkTypes :: WholeProgram -> CompilationUnit -> AstNode -> Either String ()
checkTypes program unit (Node x _) = analyze ctx x
  where
    ctx = TypeAnalysis {
      ctxLocalEnv = Map.empty,
      ctxThis     = Nothing,
      ctxReturn   = Void,
      ctxProgram  = program,
      ctxUnit     = unit }

-- For local types only.
type LocalEnvironment = Map.Map String Type

data TypeAnalysis = TypeAnalysis
  { ctxLocalEnv :: LocalEnvironment
  , ctxThis     :: Maybe Name
  , ctxReturn   :: Type
  , ctxProgram  :: WholeProgram
  , ctxUnit     :: CompilationUnit -- TODO: this last field is temporary
  }

-- Analysis on classes/methods/statements. No Type is returned.
instance Analysis TypeAnalysis () where
  analyze ctx a@(AstTypeDeclaration x) =
    propagateAnalyze ctx{ ctxThis = Just $ typeCanonicalName x } a

  analyze ctx a@(AstField v) = do
    sourceType <- analyze' ctx (variableValue v)
    let targetType = variableType v
    when (not $ isTypeAssignable (ctxProgram ctx) targetType sourceType)
      (Left $ "Not type assignable (" ++ show sourceType ++ " to " ++ show targetType ++ ")")
    -- `this` is inaccessible in static fields.
    let newThis = if isFieldStatic v then Nothing else ctxThis ctx
    propagateAnalyze ctx{ ctxThis = newThis } a

  analyze ctx (AstMethod m@Method{}) = do
    let newCtx = ctx {
      ctxLocalEnv = Map.fromList
        [(variableName param, variableType param) | param <- methodParameters m],
      -- `this` is inaccessible in static methods.
      ctxThis     = if isMethodStatic m then Nothing else ctxThis ctx,
      ctxReturn   = methodReturn m }

    analyze' newCtx (methodStatement m)

  analyze ctx (AstStatement s@ExpressionStatement{nextStatement=n}) = do
    -- The type annotation is required here because the return is ignored.
    exprType <- analyze' ctx (statementExpression s) :: Either String Type
    analyze' ctx n

  analyze ctx x@(AstStatement s@LoopStatement{nextStatement=n}) = do
    predicateType <- analyze' ctx (loopPredicate s)
    when (not $ isBoolean $ predicateType)
      (Left "Loop predicate must be a boolean")
    propagateAnalyze ctx x

  analyze ctx x@(AstStatement s@IfStatement{nextStatement=n}) = do
    predicateType <- analyze' ctx (ifPredicate s)
    when (not $ isBoolean $ predicateType)
      (Left "If predicate must be a boolean")
    propagateAnalyze ctx x

  analyze ctx (AstStatement ReturnStatement{returnExpression=Nothing, nextStatement=n}) = do
    let returnType = ctxReturn ctx
    when (returnType /= Void) (Left $ "Cannot return nothing, expected " ++ show returnType)
    analyze' ctx n

  analyze ctx (AstStatement ReturnStatement{returnExpression=Just e, nextStatement=n}) = do
    sourceType <- analyze' ctx e
    let targetType = ctxReturn ctx
    when (not $ isTypeAssignable (ctxProgram ctx) targetType sourceType)
      (Left $ "Not type assignable (" ++ show sourceType ++ " to " ++ show targetType ++ ")")
    analyze' ctx n

  analyze ctx (AstStatement LocalStatement{localVariable=v, nextStatement=n}) = do
    sourceType <- analyze' ctx (variableValue v)
    let targetType = variableType v
    when (not $ isTypeAssignable (ctxProgram ctx) targetType sourceType)
      (Left $ "Not type assignable (" ++ show sourceType ++ " to " ++ show targetType ++ ")")
    let newEnv = Map.insert (variableName v) (variableType v) (ctxLocalEnv ctx)
    analyze' ctx{ ctxLocalEnv = newEnv } n

  analyze ctx x = propagateAnalyze ctx x

-- Analysis on expressions. Type is returned.
instance Analysis TypeAnalysis Type where
  -- Binary Operations
  analyze ctx (AstExpression e@(BinaryOperation op expr1 expr2))

    -- JLS 15.17: Multiplicative Operators (*, /, %)
    | op `elem` [Multiply, Divide, Modulus] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if isNumeric expr1Type && isNumeric expr2Type
        then return (Type Int False)
        else Left ("Bad multiplicative types (" ++
          show expr1Type ++
          show op ++
          show expr2Type ++ ")")

    -- JLS 15.18.1: String Concatenation Operator (+)
    -- JLS 15.18.2: Additive Operators (+) for Numeric Types
    | op `elem` [Add] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if isString expr1Type || isString expr2Type
        then return (Type (NamedType ["java", "lang", "String"]) False)
        else if isNumeric expr1Type && isNumeric expr2Type
          then return (Type Int False) -- TODO: promotions
          else Left ("Bad additive types (" ++
            show expr1Type ++
            show op ++
            show expr2Type ++ ")")

    -- JLS 15.18.2: Additive Operators (-) for Numeric Types
    | op `elem` [Subtract] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if isNumeric expr1Type && isNumeric expr2Type
        then return (Type Int False)
        else Left ("Bad additive types (" ++
          show expr1Type ++
          show op ++
          show expr2Type ++ ")")

    -- JLS 15.20: Relational Operators (<, >, <=, >=)
    | op `elem` [Less, Greater, LessEqual, GreaterEqual] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if (isNumeric expr1Type && isNumeric expr2Type)
        then return (Type Boolean False) -- TODO: more checks are required
        else Left ("Bad relational types (" ++
          show expr1Type ++
          show op ++
          show expr2Type ++ ")")

    -- JLS 15.21: Equality Operators (==, !=)
    | op `elem` [Equality, Inequality] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if (isNumeric expr1Type && isNumeric expr2Type) ||
          (isBoolean expr1Type && isBoolean expr2Type) ||
          ((isReference expr1Type && isReference expr2Type) &&
            (isTypeCastable (ctxProgram ctx) expr1Type expr2Type ||
            isTypeCastable (ctxProgram ctx) expr2Type expr1Type))
        then return (Type Boolean False)
        else Left ("Bad equality types (" ++
          show expr1Type ++
          show op ++
          show expr2Type ++ ")")

    -- JLS 15.22.2: Boolean Logical Operators &, ^, and |
    -- JLS 15.23: Conditional-And Operator (&&)
    -- JLS 15.24: Conditional-Or Operator (||)
    | op `elem` [LazyAnd, LazyOr, And, Or] = do
      expr1Type <- analyze' ctx expr1
      expr2Type <- analyze' ctx expr2
      if (isBoolean expr1Type && isBoolean expr2Type)
        then return (Type Boolean False)
        else Left ("Bad conditional types (" ++
          show expr1Type ++
          show op ++
          show expr2Type ++ ")")

    -- JLS 15.26: Assignment Operators (=)
    | op `elem` [Assign] = do
      targetType <- analyze' ctx expr1
      sourceType <- analyze' ctx expr2
      when (not $ isTypeAssignable (ctxProgram ctx) targetType sourceType)
        (Left $ "Not type assignable (" ++ show sourceType ++ " to " ++ show targetType ++ ")")
      return targetType

  -- JLS 15.15.4: Unary Minus Operator (-)
  analyze ctx (AstExpression e@(UnaryOperation Negate expr)) = do
    exprType <- analyze' ctx expr
    if not $ isNumeric exprType
      then Left ("Can only negate numeric types (-" ++ show exprType ++ ")")
      else return (Type Int False) -- promotion

  -- JLS 15.15.6: Logical Complement Operator (!)
  analyze ctx (AstExpression e@(UnaryOperation Not expr)) = do
    exprType <- analyze' ctx expr
    if not $ isBoolean exprType
      then Left ("Can only not boolean types (!" ++ show exprType ++ ")")
      else return exprType

  -- JLS 15.8.1: Lexical Literals
  analyze ctx (AstExpression (LiteralExpression t))
    = Right (literalType t)

  -- JLS 15.8.3: this
  analyze TypeAnalysis{ctxThis=Nothing} (AstExpression This)
    = Left ("Not a valid location for the this keyword")
  analyze TypeAnalysis{ctxThis=Just name} (AstExpression This)
    = Right (Type (NamedType name) False)

  -- TODO: This is a hack. Once disambiguation works properly, remove this.
  analyze ctx (AstExpression (ExpressionName name))
    = Left "TODO: Once disambiguation works properly, remove this"

  -- JLS 15.9: Class Instance Creation Expressions
  analyze ctx (AstExpression e@(NewExpression name argumentExprs)) = do
    classDecl <- case resolveTypeInProgram (ctxProgram ctx) name of
      Just t  -> Right t
      Nothing -> Left ("Could not resolve " ++ show name ++ " in program")
    argumentTypes <- foldEither $ map (analyze' ctx) argumentExprs
    let constructor = findOverload "" argumentTypes (constructors classDecl)
    case constructor of
      Just m  -> Right (Type (NamedType $ typeCanonicalName classDecl) False)
      Nothing -> Left ("Could not find a matching constructor for " ++
        show name ++ " with arguments " ++ show argumentTypes)

  -- JLS 15.10: Array Creation Expressions
  analyze ctx (AstExpression e@(NewArrayExpression t sizeExpr)) = do
    sizeType <- analyze' ctx sizeExpr
    if isNumeric sizeType
      then return (toArray t)
      else Left ("Array size must be numeric (" ++ show sizeType ++ ")")

  -- JLS 15.16: Cast Expressions
  analyze ctx (AstExpression e@(CastExpression targetType expr)) = do
    sourceType <- analyze' ctx expr :: Either String Type
    when (not $ isTypeCastable (ctxProgram ctx) targetType sourceType)
      (Left $ "Not cast assignable (" ++ show sourceType ++ " to " ++ show targetType ++ ")")
    return $ targetType

  -- JLS 15.20.2: Type Comparison Operator instanceof
  analyze ctx (AstExpression e@(InstanceOfExpression expr targetType)) = do
    sourceType <- analyze' ctx expr
    when (not $ isReference sourceType && isReference targetType)
      (Left $ "Bad instanceof operator (" ++ show sourceType ++ " instanceof " ++ show targetType ++ ")")
    when (not $ isTypeCastable (ctxProgram ctx) targetType sourceType)
      (Left $ "Not cast assignable (" ++ show sourceType ++ " to " ++ show targetType ++ ")")
    return $ Type Boolean False

  -- JLS 15.13: Array Access Expressions
  analyze ctx (AstExpression e@(ArrayExpression arrayExpr idxExpr)) = do
    arrayType <- analyze' ctx arrayExpr
    idxExpr <- analyze' ctx idxExpr
    if not $ isArray arrayType
      then Left ("Can only perform array access on an array (" ++ show arrayType ++ ")")
      else do
        if not $ isNumeric idxExpr
          then Left ("Can only perform array access with numbers (" ++ show idxExpr ++ ")")
          else return (toScalar arrayType)

  -- TODO: This is a hack. Once disambiguation works properly, remove this.
  analyze ctx (AstExpression e@(AmbiguousFieldAccess primary name)) =
    Left "TODO: Once disambiguation works properly, remove this."

  -- JLS 15.12: Method Invocation Expressions (dynamic)
  analyze ctx (AstExpression e@(DynamicMethodInvocation expr name argExprs)) = do
    exprType <- analyze' ctx expr
    when (not $ isName exprType)
      (Left $ "Method may only be invoked on reference types " ++ show e)
    argTypes <- foldEither $ map (analyze' ctx) argExprs
    let className = getTypeName exprType
    method <- case findDynamicMethodInProgram (ctxProgram ctx) className name argTypes of
      Just m  -> Right m
      Nothing -> Left $ "Cannot find static method " ++ name ++ " in " ++ showName className ++
        " with arguments " ++ show argTypes
    return (methodReturn method)

  -- JLS 15.11: Field Access Expressions (dynamic)
  analyze ctx (AstExpression (DynamicFieldAccess e name)) = do
    classType <- analyze' ctx e
    if (isArray classType && name == "length")
    then (Right $ Type Int False) -- special case for array.length
    else do
      when (not $ isName classType)
        (Left $ "Cannot access field of non-class type " ++ name)
      cu <- case resolveUnitInProgram (ctxProgram ctx) (getTypeName classType) of
        Just x  -> Right x
        Nothing -> Left $ ("Could not resolve type " ++ show classType ++
          "; This should have been checked by disambiguation.")
      field <- case findDynamicFieldInUnit (ctxProgram ctx) cu name of -- TODO: last?
        Just x  -> Right x
        Nothing -> Left $ ("Could not resolve field " ++ name ++
          " in " ++ show classType ++
          "; This should have been checked by disambiguation.")
      if isFieldStatic field
      then Left "Cannot dynamically access static field"
      else Right (variableType field)

  -- JLS 15.12: Method Invocation Expressions (static)
  analyze ctx (AstExpression (StaticMethodInvocation className name argExprs)) = do
    argTypes <- foldEither $ map (analyze' ctx) argExprs
    method <- case findStaticMethodInProgram (ctxProgram ctx) className name argTypes of
      Just m  -> Right m
      Nothing -> Left $ "Cannot find static method " ++ name ++ " in " ++ showName className ++
        " with arguments " ++ show argTypes
    return (methodReturn method)

  -- JLS 15.11: Field Access Expressions (static)
  analyze ctx (AstExpression (StaticFieldAccess name)) = do
    let field = getStaticFieldInType (ctxProgram ctx) (getTypeInProgram (ctxProgram ctx) (init name)) (last name)
    Right $ variableType field

  analyze ctx (AstExpression (LocalAccess name)) =
    case maybeLocalType of
      Nothing -> Left ("Could not find local type '" ++ name ++ "'")
      Just x  -> Right x
    where maybeLocalType = Map.lookup name (ctxLocalEnv ctx)

  analyze _ e = Left ("Invalid analysis expression: " ++ show e)


---------- Helper Functions ----------

-- See JLS 5.2: Assignment Conversion
-- Additionally, a narrowing conversion are not allowed in Joos.
isTypeAssignable :: WholeProgram -> Type -> Type -> Bool
isTypeAssignable wp target source
  | target == Void || source == Void         = False -- Void is not a real type
  | target == source                         = True  -- Identity conversion
  | canNumericWiden source target            = True  -- Widening primitive conversion
  | isReference source && isReference target = isReferenceAssignable wp target source
  | otherwise                                = False

-- See JLS 5.5: Cast Conversion
isTypeCastable :: WholeProgram -> Type -> Type -> Bool
isTypeCastable wp target source
  | target == Void || source == Void         = False -- Void is not a real type
  | target == source                         = True  -- Identity conversion
  | canNumericWiden source target            = True  -- Widening primitive conversion
  | canNumericNarrow source target           = True  -- Narrowing primitive conversion
  | isReference source && isReference target = isReferenceAssignable wp target source ||
                                               isReferenceAssignable wp source target
  | otherwise                                = False

-- Joos uses this simplification in assignment, casting and instanceof.
isReferenceAssignable :: WholeProgram -> Type -> Type -> Bool
isReferenceAssignable wp t@(Type (NamedType tName) tArr) s@(Type (NamedType sName) sArr)
  | s == t                                = True  -- Identity conversion
  | sArr && tArr                          = isReferenceAssignable wp (toScalar t) (toScalar s)
  | not sArr && tArr                      = False -- Cannot assign class/interface type to array
  | sArr && tName `elem` arrayAssignables = True  -- Can assign array to limited interfaces
  | not sArr && not tArr && tName `elem` sourceHierarchy = True  -- Widening reference conversion
  | otherwise                             = False
  where sourceHierarchy = typeHierarchyNames wp sName
isReferenceAssignable _ (Type (NamedType ["java", "lang", "Object"]) False) s = isReference s
isReferenceAssignable _ t Null = isReference t -- Can assign null to reference type
isReferenceAssignable _ (Type t True) (Type s True) = t == s -- Arrays of primitives
isReferenceAssignable _ target source = False

-- See JLS 5.1.2: Widening Primitive Conversion
-- source -> target -> bool
canNumericWiden :: Type -> Type -> Bool
canNumericWiden (Type Byte False)  (Type Short False) = True
canNumericWiden (Type Byte False)  (Type Int False)   = True
canNumericWiden (Type Short False) (Type Int False)   = True
canNumericWiden (Type Char False)  (Type Int False)   = True
canNumericWiden _                  _                  = False

-- See JLS 5.1.2: Narrowing Primitive Conversion
-- source -> target -> bool
canNumericNarrow :: Type -> Type -> Bool
canNumericNarrow (Type Byte False)  (Type Char False)  = True
canNumericNarrow (Type Short False) (Type Byte False)  = True
canNumericNarrow (Type Short False) (Type Char False)  = True
canNumericNarrow (Type Char False)  (Type Byte False)  = True
canNumericNarrow (Type Char False)  (Type Short False) = True
canNumericNarrow (Type Int False)   (Type Byte False)  = True
canNumericNarrow (Type Int False)   (Type Short False) = True
canNumericNarrow (Type Int False)   (Type Char False)  = True
canNumericNarrow _                  _                  = False

-- Classes/interfaces you can assign an array to.
arrayAssignables :: [Name]
arrayAssignables = [
  ["java", "lang", "Object"],
  ["java", "io", "Serializable"],
  ["java", "lang", "Cloneable"]]

toScalar :: Type -> Type
toScalar (Type x _) = Type x False
toScalar Null = Null
toScalar Void = Void

toArray :: Type -> Type
toArray (Type x _) = Type x True
toArray Null = Null
toArray Void = Void

isName :: Type -> Bool
isName (Type (NamedType _) False) = True
isName _                          = False

getTypeName :: Type -> Name
getTypeName (Type (NamedType n) False) = n
getTypeName _ = error "getTypeName called on wrong Type (not NamedType)"

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
