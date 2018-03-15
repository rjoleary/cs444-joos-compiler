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
  show WholeProgram{} = "WholeProgram"

instance Show SubPackage where
  show (SubPackage x y) = "Subpackage: " ++ show x ++ " " ++ show y

instance Show Package where
  show (Package name subs units) =
    "Package: n=" ++
    (showName name) ++
    (if length subs > 0
       then " subs(" ++
            (commaDelimit $
             map fst subs) ++
            ")"
       else "") ++
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
  show (TypeDeclaration name _modifiers _isInterface _super _interfaces fields _methods _) =
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
    (commaDelimit $ map fieldName fields) ++
    ")"

instance Show Field where
  show (Field _type _modifiers _name _) =
    "Field: " ++
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
      locals = scopeLocals s
      localNames = map localName locals


instance Show Statement where
  show Statement{statement=s} = show s

instance Show InnerStatement where
  show BlockStatement{} = "BlockStatement"
  show ExpressionStatement{} = "ExpressionStatement"
  show LoopStatement{} = "LoopStatement"
  show IfStatement{} = "IfStatement"
  show ReturnStatement{} = "Return"
  show EmptyStatement{} = "EmptyStatement"
  show LocalStatement{} = "LocalStatement"

instance Show Local where
  show (Local _type _modifiers _name _) =
    "Local: " ++  m ++ show _type ++ " " ++ _name
    where
      m =
        if length _modifiers > 0
          then (intercalate " " $ map show _modifiers) ++ " "
          else ""

instance Show Expression where
  show Expression{innerExpression=e} = show e

instance Show InnerExpression where
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
  show (IntegerLiteral x)   = show x
  show (BooleanLiteral x)   = show x
  show (CharacterLiteral x) = show x
  show (StringLiteral x)    = show x
  show NullLiteral          = "null"

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

extractTypeName :: Maybe TypeDeclaration -> String
extractTypeName Nothing  = "N/A"
extractTypeName (Just c) = typeName c

emptyScope :: InnerStatement -> Statement
emptyScope s = Statement{ statement = s }

emptyType :: InnerExpression -> Expression
emptyType e = Expression{ expressionType = Void, innerExpression = e }

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
    parameterTypes = map (show . localType) (methodParameters x)

-- Creates a type signature from the type name.
typeSignature :: Type -> String
typeSignature x = show x

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