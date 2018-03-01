module JoosCompiler.Ast.NodeTypes where

import           Data.List
import           Data.Maybe

type Name = [String]

data Modifier
  = Public
  | Protected
  | Final
  | Abstract
  | Static
  | Native
  deriving (Show)

data WholeProgram =
  WholeProgram [CompilationUnit]
  deriving (Show)

data CompilationUnit
  = CompilationUnit { package   :: Maybe Name
                    , imports   :: [ImportDeclaration]
                    , classDecl :: Maybe ClassDeclaration }
  | EmptyFile

instance Show CompilationUnit where
  show (CompilationUnit p i c) =
    "CompilationUnit(p=" ++
    (showName $ fromMaybe ["N/A"] p) ++
    " i=[" ++
    (intercalate ", " $ map (showName . importName) i) ++
    "]" ++ " c=" ++ extractClassName c ++ ")"
  show EmptyFile = "EmptyFile"

data PackageDeclaration = PackageDeclaration
  { packageName :: Name
  } deriving (Show)

data ImportDeclaration = ImportDeclaration
  { importName :: Name
  , onDemand   :: Bool
  } deriving (Show)

data Block = Block
  { blockScope :: Scope
  } deriving (Show)

data Field = Field
  { fieldType      :: Type
  , fieldModifiers :: [Modifier]
  , fieldName      :: Name
  , fieldValue     :: Expression
  , isClassField   :: Bool
  }

instance Show Field where
  show (Field _type _modifiers _name _ _) =
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
  , constructor    :: Maybe Method
  }

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
    ") Methods(" ++
    (intercalate ", " (map (showName . methodName) _methods)) ++ ")"

data Method = Method
  { methodType       :: Type
  , methodModifiers  :: [Modifier]
  , methodName       :: Name
  , formalParameters :: [Field]
  , statements       :: [Statement]
  } deriving (Show)

data Expression
  = MethodInvocation { functionName :: Name
                     , arguments    :: [Expression] }
  | BinOpApplication { operator     :: Operator
                     , leftOperand  :: Expression
                     , rightOperand :: Expression }
  | UnaryOpApplication { operator :: Operator
                       , operand  :: Expression }
  | Literal { value :: String }
  deriving (Show)

data Scope = Scope
  { fields      :: [Field]
  , parentScope :: Maybe Scope
  } deriving (Show)

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
  deriving (Show)

data Statement = Statement
  { scope     :: Scope
  , statement :: InnerStatement
  } deriving (Show)

data InnerType
  = Boolean
  | Byte
  | Char
  | Int
  | Short
  | NamedType { unNamedType :: Name }
  deriving (Show)

data Type
  = Void
  | Type { innerType :: InnerType
         , isArray   :: Bool }

instance Show Type where
  show (Type _type _isArray) =
    show _type ++
    (if _isArray
       then "[]"
       else "")
  show Void = "Void"

data Operator
  = Plus
  | Minus
  | Divide
  | Mod
  | Negate
  deriving (Show)

showName :: [String] -> String
showName l = intercalate "." l

extractClassName :: Maybe ClassDeclaration -> String
extractClassName Nothing  = "N/A"
extractClassName (Just c) = className c
