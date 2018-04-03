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
  deriving (Eq, Ord)

data WholeProgram = WholeProgram
  { programPackages :: [Package]
  , programCus :: [CompilationUnit]
  } deriving (Eq)

data PackageDeclaration = PackageDeclaration
  { packageDeclarationName :: Name
  } deriving (Eq, Show)

type PackageCompilationUnits = [String]

data Package = Package
  { packageName             :: Name
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
  { typeName             :: String
  , classModifiers       :: [Modifier]
  , isInterface          :: Bool
  , super                :: Name
  , interfaces           :: [Name]
  , classFields          :: [Variable]
  , methods              :: [Method]
  , constructors         :: [Method]
  , typeCanonicalName    :: Name
  } deriving (Eq)

data Variable = Variable
  { variableType            :: Type
  , variableModifiers       :: [Modifier]
  , variableName            :: String
  , variableValue           :: Expression
  , variableCanonicalName   :: Name
  } deriving (Eq)

-- The methodReturn for a constructor is always void.
data Method = Method
  { methodReturn           :: Type
  , methodModifiers        :: [Modifier]
  , methodName             :: String
  , methodParameters       :: [Variable]
  , methodStatement        :: Statement
  , methodCanonicalName    :: Name
  } deriving (Eq)


---------- Statements and Expressions ----------

data Block = Block
  { blockScope :: Scope
  } deriving (Eq)

newtype StatementKind a = Statement a

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
  = This

  | BinaryOperation BinaryOperator Expression Expression
  | UnaryOperation UnaryOperator Expression

  | LiteralExpression Literal

  | NewExpression Name [Expression]
  | NewArrayExpression Type Expression

  | CastExpression Type Expression
  | InstanceOfExpression Expression Type

  | ArrayExpression Expression Expression

  | DynamicMethodInvocation Expression String [Expression]
  | StaticMethodInvocation Name String [Expression]

  -- Ambiguous accesses
  | ExpressionName Name
  | AmbiguousFieldAccess Expression String

  -- Non-ambiguous accesses
  | DynamicFieldAccess Expression String
  | StaticFieldAccess Name
  | LocalAccess String
  -- Only used as an intermediate for StaticMethodAccess
  | ClassAccess Name
  deriving (Eq)

data Type
  = Void
  | Null
  | Type InnerType Bool -- True = array, False = scalar
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
