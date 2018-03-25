module JoosCompiler.Ast.NodeFunctions where

import JoosCompiler.Ast.NodeTypes

import Data.List
import Data.Maybe

---------- Show ----------

instance Show Modifier where
  show Public    = "public"
  show Protected = "protected"
  show Final     = "final"
  show Abstract  = "abstract"
  show Static    = "static"
  show Native    = "native"

instance Show WholeProgram where
  show WholeProgram{programPackages = p} = "WholeProgram. Packages: " ++ show p

instance Show Package where
  show (Package name units) =
    "Package: n=" ++
    (showName name) ++
    (if length units > 0
       then " units(" ++ commaDelimit units ++ ")"
       else "")

instance Show CompilationUnit where
  show (CompilationUnit p i c _) =
    "CompilationUnit(p=" ++
    (showName p) ++
    " i=[" ++
    (commaDelimit $ map (showName . importName) i) ++
    "]" ++ " c=" ++ extractTypeName c ++ ")"
  show EmptyFile = "EmptyFile"

instance Show ImportDeclaration where
  show i@ImportDeclaration{onDemand=True} = (showName $ importName i) ++ ".*"
  show i                                  = showName $ importName i

instance Show TypeDeclaration where
  show (TypeDeclaration name _modifiers _isInterface _super _interfaces fields _methods _ _) =
    "TypeDeclaration: " ++
    show _modifiers ++
    " " ++
    (if _isInterface
       then "interface"
       else "class") ++
    " " ++
    name ++
    (if (length _interfaces) > 0
       then " implements(" ++ (commaDelimit $ map showName _interfaces) ++ ")"
       else "") ++
    " extends(" ++
    (showName _super) ++
    ") Fields(" ++
    (commaDelimit $ map variableName fields) ++
    ")"

instance Show Variable where
  show (Variable _type _modifiers _name _ _) =
    "Variable: " ++
    m ++ show _type ++ " " ++ _name
    where
      m =
        if length _modifiers > 0
          then (intercalate " " $ map show _modifiers) ++ " "
          else ""

instance Show Method where
  show m@Method{methodReturn=r} = "Method: " ++ show r ++ " " ++ methodSignature m

instance Show Block where
  show Block{blockScope=s} = "Block: Locals(" ++commaDelimit localNames ++ ")"
    where
      locals = scopeVariables s
      localNames = map variableName locals

instance Show Statement where
  show BlockStatement{} = "BlockStatement"
  show ExpressionStatement{} = "ExpressionStatement"
  show LoopStatement{} = "LoopStatement"
  show IfStatement{} = "IfStatement"
  show ReturnStatement{} = "Return"
  show EmptyStatement{} = "EmptyStatement"
  show LocalStatement{localVariable=v} = "LocalStatement " ++ show v
  show TerminalStatement{} = "TerminalStatement"

instance Show Expression where
  show (MethodInvocation e name args) = "(" ++ show e ++ "." ++ name ++ "(" ++ intercalate "," (map show args) ++ "))"
  show (BinaryOperation op e1 e2)     = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (UnaryOperation op e)          = "(" ++ show op ++ show e ++ ")"
  show (LiteralExpression v)          = "(" ++ show v ++ ")"
  show This                           = "(this)"
  show (FieldAccess e s)              = "(" ++ show e ++ "." ++ s ++ ")"
  show (ExpressionName n)             = "(" ++ showName n ++ ")"
  show (NewExpression name args)      = "(new " ++ showName name ++ "(" ++ intercalate "," (map show args) ++ "))"
  show (NewArrayExpression t e)       = "(new " ++ typeSignature t ++ "[" ++ show e ++ "])"
  show (CastExpression t e)           = "((" ++ typeSignature t ++ ")" ++ show e ++ ")"
  show (InstanceOfExpression e t)     = "(" ++ show e ++ " instanceof " ++ typeSignature t ++ ")"
  show (ArrayExpression e1 e2)        = "(" ++ show e1 ++ "[" ++ show e2 ++ "])"

instance Show Type where
  show Void           = "void"
  show Null           = "null"
  show (Type x False) = show x
  show (Type x True)  = show (Type x False) ++ "[]"

instance Show InnerType where
  show Boolean       = "boolean"
  show Byte          = "byte"
  show Char          = "char"
  show Int           = "int"
  show Short         = "short"
  show (NamedType x) = showName x

instance Show Literal where
  show (IntegerLiteral x)     = show x
  show (BooleanLiteral True)  = "true"
  show (BooleanLiteral False) = "false"
  show (CharacterLiteral x)   = show x
  show (StringLiteral x)      = show x
  show NullLiteral            = "null"

instance Show BinaryOperator where
  show Multiply     = "*"
  show Divide       = "/"
  show Modulus      = "%"
  show Add          = "+"
  show Subtract     = "-"
  show Less         = "<"
  show Greater      = ">"
  show LessEqual    = "<="
  show GreaterEqual = ">="
  show Equality     = "=="
  show Inequality   = "!="
  show LazyAnd      = "&"
  show LazyOr       = "|"
  show And          = "&&"
  show Or           = "||"
  show Assign       = "="

instance Show UnaryOperator where
  show Negate = "-"
  show Not    = "!"

---------- Mapping over statements/expressions ----------

mapStatement :: (Statement -> Statement) -> Statement -> Statement
mapStatement _ TerminalStatement = TerminalStatement

mapStatement f x@(BlockStatement s _) =
  applyNextMapStatement f x{ statementBlock = mapStatement f s }

mapStatement f x@(LoopStatement _ s _) =
  applyNextMapStatement f x{ loopStatement = mapStatement f s }

mapStatement f x@(IfStatement _ t e _) =
  applyNextMapStatement f x{ ifThenStatement = newThen
                           , ifElseStatement = newElse
                           }
  where
    newThen = mapStatement f t
    newElse = mapStatement f e

-- ExpressionStatement
-- ReturnExpression
-- LocalStatement
-- EmptyStatement
-- TerminalStatement
mapStatement f x = applyNextMapStatement f x

applyNextMapStatement :: (Statement -> Statement) -> Statement -> Statement
applyNextMapStatement _ TerminalStatement = TerminalStatement
applyNextMapStatement f s
  | newStatement /= TerminalStatement =
    newStatement{ nextStatement = newNext }
  | otherwise = newStatement
  where
    newStatement = f s
    newNext = mapStatement f $ nextStatement newStatement

mapStatementExpression :: (Expression -> Expression) -> Statement -> Statement

mapStatementExpression _ TerminalStatement = TerminalStatement

mapStatementExpression f old@ExpressionStatement{statementExpression = e} =
  old{ statementExpression = mapExpression f e
     , nextStatement = next
     }
  where
    next = mapStatement (mapStatementExpression f) $ nextStatement old

mapStatementExpression f old@LoopStatement{loopPredicate = p} =
  old{ loopPredicate = mapExpression f p
     , nextStatement = next
     }
  where
    next = mapStatement (mapStatementExpression f) $ nextStatement old

mapStatementExpression f old@IfStatement{ifPredicate = p} =
  old{ ifPredicate = mapExpression f p
     , nextStatement = next
     }
  where
    next = mapStatement (mapStatementExpression f) $ nextStatement old

mapStatementExpression f old@ReturnStatement{ returnExpression = Just e } =
  old{ returnExpression = Just $ mapExpression f e
     , nextStatement = next }
  where
    next = mapStatement (mapStatementExpression f) $ nextStatement old

mapStatementExpression f old =
  old{ nextStatement = next }
  where
    next = mapStatement (mapStatementExpression f) $ nextStatement old

mapExpression :: (Expression -> Expression) -> Expression -> Expression
mapExpression = id
-- mapExpression f (MethodInvocation e1 s le2) =
--   MethodInvocation (f e1) s (map f le2)
-- mapExpression f (BinaryOperation o e1 e2) =
--   BinaryOperation o (f e1) (f e2)
-- mapExpression f (UnaryOperation o e) =
--   UnaryOperation o (f e)
-- mapExpression f e@LiteralExpression{} = f e
-- mapExpression f This = f This

---------- Other Functions ----------

importName :: ImportDeclaration -> Name
importName ImportDeclaration{onDemand=False, importPackageName=p, importTypeName=t} =
  p ++ [fromJust t]
importName ImportDeclaration{onDemand=True, importPackageName=p} = p

showName :: [String] -> String
showName l = intercalate "." l

literalType :: Literal -> Type
literalType IntegerLiteral{}   = Type Int False
literalType BooleanLiteral{}   = Type Boolean False
literalType CharacterLiteral{} = Type Char False
literalType StringLiteral{}    = Type (NamedType ["java", "lang", "String"]) False
literalType NullLiteral{}      = Null

-- Returns the default value for fields if they are left uninitialized.
unitializedLiteral :: Type -> Literal
unitializedLitearl (Type (NamedType _) _) = NullLiteral
unitializedLiteral (Type _ True)          = NullLiteral
unitializedLiteral (Type Boolean _)       = BooleanLiteral False
unitializedLiteral (Type _ _)             = IntegerLiteral 0

extractTypeName :: Maybe TypeDeclaration -> String
extractTypeName Nothing  = "N/A"
extractTypeName (Just c) = typeName c

commaDelimit :: [String] -> String
commaDelimit l = intercalate ", " l

isClassFinal :: TypeDeclaration -> Bool
isClassFinal x = Final `elem` classModifiers x

-- Create a method signature from the method name and parameter types. The
-- return type is omitted.
-- TODO: types must be canonical beforehand
methodSignature :: Method -> String
methodSignature x = name ++ "(" ++ commaDelimit parameterTypes ++ ")"
  where
    name = methodName x
    parameterTypes = map (show . variableType) (methodParameters x)

-- Creates a type signature from the type name.
typeSignature :: Type -> String
typeSignature x = show x

isFieldStatic :: Variable -> Bool
isFieldStatic x = Static `elem` variableModifiers x

isMethodFinal :: Method -> Bool
isMethodFinal x = Final `elem` methodModifiers x

isMethodPublic :: Method -> Bool
isMethodPublic x = Public `elem` methodModifiers x

isMethodProtected :: Method -> Bool
isMethodProtected x = Protected `elem` methodModifiers x

isMethodStatic :: Method -> Bool
isMethodStatic x = Static `elem` methodModifiers x

isMethodAbstract :: Method -> Bool
isMethodAbstract x = Abstract `elem` methodModifiers x
