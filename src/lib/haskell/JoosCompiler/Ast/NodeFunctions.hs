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
  show (SubPackage x y) = show x ++ " " ++ show y

instance Show Package where
  show (Package name subs _) =
    "n=" ++
    (showName name) ++
    (if length subs > 0
       then " subs(" ++
            (intercalate ", " $
             map fst subs) ++
            ")"
       else "")

instance Show CompilationUnit where
  show (CompilationUnit p i c _) =
    "CompilationUnit(p=" ++
    (showName $ fromMaybe ["N/A"] p) ++
    " i=[" ++
    (intercalate ", " $ map (showName . importName) i) ++
    "]" ++ " c=" ++ extractTypeName c ++ ")"
  show EmptyFile = "EmptyFile"

instance Show ImportDeclaration where
  show ImportDeclaration{importName=n, onDemand=True} = show n ++ ".*"
  show ImportDeclaration{importName=n}                = show n

instance Show TypeDeclaration where
  show (TypeDeclaration name _modifiers _isInterface _super _interfaces fields _methods _) =
    show _modifiers ++
    " " ++
    (if _isInterface
       then "interface"
       else "class") ++
    " " ++
    name ++
    (if (length _interfaces) > 0
       then " implements(" ++ (show $ map showName _interfaces) ++ ")"
       else "") ++
    " extends(" ++
    (showName _super) ++
    ") Fields(" ++
    (intercalate ", " $ map (showName . fieldName) fields) ++
    ")"

instance Show Field where
  show (Field _type _modifiers _name _) =
    m ++ show _type ++ " " ++ showName _name
    where
      m =
        if length _modifiers > 0
          then (intercalate " " $ map show _modifiers) ++ " "
          else ""

instance Show Method where
  show m@Method{methodReturn=r} = show r ++ " " ++ methodSignature m

instance Show Block where
  show Block{} = "Block"

instance Show Statement where
  show Statement{} = "Statement"

instance Show Local where
  show (Local _type _modifiers _name _) =
    m ++ show _type ++ " " ++ showName _name
    where
      m =
        if length _modifiers > 0
          then (intercalate " " $ map show _modifiers) ++ " "
          else ""

instance Show Expression where
  show (MethodInvocation name args) = show name ++ "(...)"
  show (BinaryOperation op e1 e2)   = show op
  show (UnaryOperation op e)        = show op
  show (Literal v)                  = v

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
  show InstanceOf   = "instanceof"
  show Equality     = "=="
  show Inequality   = "!="
  show LazyAnd      = "&"
  show LazyOr       = "|"
  show And          = "&&"
  show Or           = "||"
  show Assign       = "="

instance Show UnaryOperator where
  show Negate = "-"


---------- Other Functions ----------

showName :: [String] -> String
showName l = intercalate "." l

extractTypeName :: Maybe TypeDeclaration -> String
extractTypeName Nothing  = "N/A"
extractTypeName (Just c) = typeName c

isClassFinal :: TypeDeclaration -> Bool
isClassFinal x = Final `elem` classModifiers x

-- Create a method signature from the method name and parameter types. The
-- return type is omitted.
-- TODO: types must be canonical beforehand
methodSignature :: Method -> String
methodSignature x = name ++ "(" ++ intercalate "," parameterTypes ++ ")"
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

