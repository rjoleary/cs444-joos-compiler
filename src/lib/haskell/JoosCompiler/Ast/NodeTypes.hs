module JoosCompiler.Ast.NodeTypes where

import           Data.List
import           Data.Maybe

type Name = [String]


---------- Packages, Types and Methods ----------

data Modifier
  = Public
  | Protected
  | Final
  | Abstract
  | Static
  | Native
  deriving (Eq)

data WholeProgram = WholeProgram
  { programPackages :: SubPackage
  } deriving (Eq)

data SubPackage = SubPackage (Maybe Package) SubPackageMap
  deriving (Eq)
type SubPackageMapEntry = (String, SubPackage)
type SubPackageMap = [SubPackageMapEntry]

data PackageDeclaration = PackageDeclaration
  { packageDeclarationName :: Name
  } deriving (Eq, Show)

type PackageCompilationUnits = [(String, CompilationUnit)]

data Package = Package
  { packageName             :: Name
  , subPackages             :: SubPackageMap
  , packageCompilationUnits :: PackageCompilationUnits
  } deriving (Eq)

data CompilationUnit
  = CompilationUnit { cuPackage  :: Maybe Name
                    , imports    :: [ImportDeclaration]
                    , typeDecl   :: Maybe TypeDeclaration
                    , cuTypeName :: String }
  | EmptyFile
  deriving (Eq)

data ImportDeclaration = ImportDeclaration
  { importPackageName :: Name
  , importTypeName :: Maybe String
  , onDemand   :: Bool
  } deriving (Eq)

-- A type is a class or an interface.
data TypeDeclaration = TypeDeclaration
  { typeName       :: String
  , classModifiers :: [Modifier]
  , isInterface    :: Bool
  , super          :: Name
  , interfaces     :: [Name]
  , classFields    :: [Field]
  , methods        :: [Method]
  , constructors   :: [Method]
  } deriving (Eq)

data Field = Field
  { fieldType      :: Type
  , fieldModifiers :: [Modifier]
  , fieldName      :: Name
  , fieldValue     :: Expression
  } deriving (Eq)

-- The methodReturn for a constructor is always void.
data Method = Method
  { methodReturn     :: Type
  , methodModifiers  :: [Modifier]
  , methodName       :: String
  , methodParameters :: [Local]
  , methodStatements :: [Statement]
  } deriving (Eq)


---------- Statements and Expressions ----------

data Block = Block
  { blockScope :: Scope
  } deriving (Eq)

data Statement = Statement
  { statement :: InnerStatement
  } deriving (Eq)

data InnerStatement
  = BlockStatement { blockStatements :: [Statement] }
  | ExpressionStatement { statementExpression :: Expression }
  | LoopStatement { loopPredicate  :: Expression
                  , loopStatements :: [Statement] }
  | IfStatement { ifPredicate     :: Expression
                , ifThenStatement :: Statement
                , ifElseStatement :: Statement }
  | ReturnStatement { returnExpression :: Maybe Expression }
  | LocalStatement Local
  | EmptyStatement
  deriving (Eq)

data Scope = Scope
  { scopeLocals :: [Local]
  , parentScope :: Maybe Scope
  } deriving (Eq)

data Local = Local
  { localType      :: Type
  , localModifiers :: [Modifier]
  , localName      :: String
  , localValue     :: Expression
  } deriving (Eq)

data Expression = Expression
  { expressionType  :: Type
  , innerExpression :: InnerExpression
  } deriving(Eq)

data InnerExpression
  = MethodInvocation Expression String [Expression]
  | BinaryOperation BinaryOperator Expression Expression
  | UnaryOperation UnaryOperator Expression
  | LiteralExpression Literal
  | This
  | FieldAccess Expression String
  | ExpressionName Name
  | NewExpression Name [Expression]
  | NewArrayExpression Type Expression -- TODO: confusion on primitive types
  | CastExpression Type Expression
  | InstanceOfExpression Expression Type
  | ArrayExpression Expression Expression
  deriving (Eq)

data Type
  = Void
  | Null
  | Type { innerType :: InnerType
         , isArray   :: Bool }
  deriving (Eq)

data InnerType
  = Boolean
  | Byte
  | Char
  | Int
  | Short
  | NamedType { unNamedType :: Name }
  deriving (Eq)

data Literal
  = IntegerLiteral Integer
  | BooleanLiteral Bool
  | CharacterLiteral Char
  | StringLiteral String
  | NullLiteral
  deriving (Eq)

data BinaryOperator
  = Multiply
  | Divide
  | Modulus
  | Add
  | Subtract
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | Equality
  | Inequality
  | LazyAnd
  | LazyOr
  | And
  | Or
  | Assign
  deriving (Eq)

data UnaryOperator
  = Negate
  | Not
  deriving (Eq)
