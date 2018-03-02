module JoosCompiler.Ast.NodeTypes where

import           Data.List
import           Data.Maybe

type Name = [String]

type SubPackages = [(Name, Package)]

type PackageCompilationUnits = [(String, CompilationUnit)]

showName :: [String] -> String
showName l = intercalate "." l

extractClassName :: Maybe ClassDeclaration -> String
extractClassName Nothing  = "N/A"
extractClassName (Just c) = className c

data Modifier
  = Public
  | Protected
  | Final
  | Abstract
  | Static
  | Native
  deriving (Eq, Show)

data WholeProgram = WholeProgram
  { programPackages :: [Package]
  } deriving (Eq, Show)

data Package = Package
  { packageName             :: Maybe Name
  , subPackages             :: SubPackages
  , packageCompilationUnits :: PackageCompilationUnits
  } deriving (Eq)

instance Show Package where
  show (Package name subs _) =
    (showName $ fromMaybe ["_"] name) ++
    (if length subs > 0
       then " subs(" ++
            (intercalate ", " $
             map (showName . fromJust . packageName) $ map snd subs) ++
            ")"
       else "")

data CompilationUnit
  = CompilationUnit { cuPackage   :: Maybe Name
                    , imports     :: [ImportDeclaration]
                    , classDecl   :: Maybe ClassDeclaration
                    , cuClassName :: String }
  | EmptyFile
  deriving (Eq)

instance Show CompilationUnit where
  show (CompilationUnit p i c _) =
    "CompilationUnit(p=" ++
    (showName $ fromMaybe ["N/A"] p) ++
    " i=[" ++
    (intercalate ", " $ map (showName . importName) i) ++
    "]" ++ " c=" ++ extractClassName c ++ ")"
  show EmptyFile = "EmptyFile"

data PackageDeclaration = PackageDeclaration
  { packageDeclarationName :: Name
  } deriving (Eq, Show)

data ImportDeclaration = ImportDeclaration
  { importName :: Name
  , onDemand   :: Bool
  } deriving (Eq, Show)

data Block = Block
  { blockScope :: Scope
  } deriving (Eq, Show)

data Field = Field
  { fieldType      :: Type
  , fieldModifiers :: [Modifier]
  , fieldName      :: Name
  , fieldValue     :: Expression
  } deriving (Eq)

instance Show Field where
  show (Field _type _modifiers _name _) =
    m ++ show _type ++ " " ++ showName _name
    where
      m =
        if length _modifiers > 0
          then (intercalate " " $ map show _modifiers) ++ " "
          else ""

data Local = Local
  { localType      :: Type
  , localModifiers :: [Modifier]
  , localName      :: Name
  , localValue     :: Expression
  } deriving (Eq)

instance Show Local where
  show (Local _type _modifiers _name _) =
    m ++ show _type ++ " " ++ showName _name
    where
      m =
        if length _modifiers > 0
          then (intercalate " " $ map show _modifiers) ++ " "
          else ""

data ClassDeclaration = ClassDeclaration
  { className      :: String
  , classModifiers :: [Modifier]
  , isInterface    :: Bool
  , super          :: Name
  , interfaces     :: [Name]
  , classFields    :: [Field]
  , methods        :: [Method]
  , constructors   :: [Method]
  } deriving (Eq)

instance Show ClassDeclaration where
  show (ClassDeclaration name _modifiers _isInterface _super _interfaces fields _methods _) =
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
    ") Methods(" ++ (intercalate ", " (map methodName _methods)) ++ ")"

data Method = Method
  { methodType       :: Type
  , methodModifiers  :: [Modifier]
  , methodName       :: String
  , formalParameters :: [Local]
  , statements       :: [Statement]
  } deriving (Eq, Show)

-- Creates a method signature from the method name and parameter types. The
-- return type is omitted.
-- TODO: types must be canonical beforehand
methodSignature :: Method -> String
methodSignature x = name ++ "(" ++ intercalate "," parameterTypes ++ ")"
  where
    name = methodName x
    parameterTypes = map (show . localType) (formalParameters x)

data Expression
  = MethodInvocation { functionName :: Name
                     , arguments    :: [Expression] }
  | BinOpApplication { operator     :: Operator
                     , leftOperand  :: Expression
                     , rightOperand :: Expression }
  | UnaryOpApplication { operator :: Operator
                       , operand  :: Expression }
  | Literal { value :: String }
  deriving (Eq, Show)

data Scope = Scope
  { scopeLocals :: [Local]
  , parentScope :: Maybe Scope
  } deriving (Eq, Show)

data InnerStatement
  = AssignStatement { assignedVar   :: Name
                    , assignedValue :: Expression }
  | ExpressionStatement { statementExpression :: Expression }
  | LoopStatement { loopInit   :: Maybe Statement -- ExpressionStatement
                  , loopTest   :: Maybe Statement
                  , loopUpdate :: Maybe Statement }
  | IfStatement { ifTest        :: Expression
                , thenStatement :: Statement
                , elseStatement :: Maybe Statement }
  | EmptyStatement
  deriving (Eq, Show)

data Statement = Statement
  { scope     :: Scope
  , statement :: InnerStatement
  } deriving (Eq, Show)

data InnerType
  = Boolean
  | Byte
  | Char
  | Int
  | Short
  | NamedType { unNamedType :: Name }
  deriving (Eq)

instance Show InnerType where
  show Boolean       = "byte"
  show Byte          = "byte"
  show Char          = "char"
  show Int           = "int"
  show Short         = "short"
  show (NamedType x) = showName x

data Type
  = Void
  | Type { innerType :: InnerType
         , isArray   :: Bool }
  deriving (Eq)

instance Show Type where
  show Void           = "void"
  show (Type x False) = show x
  show (Type x True)  = show (Type x False) ++ "[]"

data Operator
  = Plus
  | Minus
  | Divide
  | Mod
  | Negate
  deriving (Eq, Show)
