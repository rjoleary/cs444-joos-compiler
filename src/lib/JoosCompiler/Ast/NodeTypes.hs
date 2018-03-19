module JoosCompiler.Ast.NodeTypes where

type Name = [String]
type Local = Variable
type Field = Variable

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
  , programCus :: [CompilationUnit]
  } deriving (Eq)

data SubPackage = SubPackage (Maybe Package) SubPackageMap
  deriving (Eq)
type SubPackageMapEntry = (String, SubPackage)
type SubPackageMap = [SubPackageMapEntry]

data PackageDeclaration = PackageDeclaration
  { packageDeclarationName :: Name
  } deriving (Eq, Show)

type PackageCompilationUnits = [String]

data Package = Package
  { packageName             :: Name
  , subPackages             :: SubPackageMap
  , packageCompilationUnits :: PackageCompilationUnits
  } deriving (Eq)

data CompilationUnit
  = CompilationUnit { cuPackage  :: Name
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
  , classFields    :: [Variable]
  , methods        :: [Method]
  , constructors   :: [Method]
  } deriving (Eq)

data Variable = Variable
  { variableType      :: Type
  , variableModifiers :: [Modifier]
  , variableName      :: String
  , variableValue     :: Expression
  } deriving (Eq)

-- The methodReturn for a constructor is always void.
data Method = Method
  { methodReturn     :: Type
  , methodModifiers  :: [Modifier]
  , methodName       :: String
  , methodParameters :: [Variable]
  , methodStatement  :: Statement
  } deriving (Eq)


---------- Statements and Expressions ----------

data Block = Block
  { blockScope :: Scope
  } deriving (Eq)

data Statement
  = BlockStatement
    { statementBlock :: Statement
    , nextStatement  :: Statement }
  | ExpressionStatement
    { statementExpression :: Expression
    , nextStatement       :: Statement }
  | LoopStatement
    { loopPredicate :: Expression
    , loopStatement :: Statement
    , nextStatement :: Statement }
  | IfStatement
    { ifPredicate     :: Expression
    , ifThenStatement :: Statement
    , ifElseStatement :: Statement
    , nextStatement   :: Statement }
  | ReturnStatement
    { returnExpression :: Maybe Expression
    , nextStatement    :: Statement }
  | LocalStatement
    { localVariable :: Variable
    , nextStatement :: Statement }
  | EmptyStatement
    { nextStatement :: Statement }
  | TerminalStatement
  deriving (Eq)

data Scope = Scope
  { scopeVariables :: [Variable]
  , parentScope    :: Maybe Scope
  , scopeCuName    :: Name
  } deriving (Eq)

data Expression
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
