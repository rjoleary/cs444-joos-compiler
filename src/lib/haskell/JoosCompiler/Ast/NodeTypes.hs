module JoosCompiler.Ast.NodeTypes where

import           Data.List

type Name = [String]

data Modifier
  = Public
  | Protected
  | Final
  | Abstract
  | Static
  | Native
  deriving (Show)

data CompilationUnit
  = CompilationUnit { package   :: Maybe Name
                    , imports   :: [Name]
                    , classDecl :: ClassDeclaration }
  | EmptyFile
  deriving (Show)

data Field = Field
  { fieldType      :: Type
  , fieldModifiers :: [Modifier]
  , fieldName      :: Name
  , fieldValue     :: Expression
  } deriving (Show)

data Scope = Scope
  { parentScope    :: Maybe Scope
  , scopeVariables :: [Field]
  , statements     :: [Statement]
  } deriving (Show)

data ClassDeclaration = ClassDeclaration
  { className      :: String
  , classModifiers :: [Modifier]
  , isInterface    :: Bool
  , super          :: Name
  , interfaces     :: [Name]
  , classScope     :: Scope
  , methods        :: [Method]
  , constructor    :: Maybe Method
  }

instance Show ClassDeclaration where
  show (ClassDeclaration name modifiers isInterface super interfaces _ _ _) =
    show modifiers ++
    " " ++
    (if isInterface
       then "interface"
       else "class") ++
    " " ++
    (showName super) ++
    " " ++
    name ++
    " " ++
    (if (length interfaces) >= 0
       then "implements" ++ " " ++ (show $ map showName interfaces) ++ " "
       else "")

data Method = Method
  { methodType      :: Type
  , methodModifiers :: [Modifier]
  , methodName      :: Name
        -- list of scopes where scope n is a parent of scope n + 1
        -- (first methodScopes) is the scope of this method's class
  , methodScopes    :: [Scope]
  } deriving (Show)

data Expression
  = MethodInvocation { functionName :: Name
                     , arguments    :: [Expression] }
  | BinOpApplication { operator     :: Operator
                     , leftOperand  :: Expression
                     , rightOperand :: Expression }
  | UnaryOpApplication { operator :: Operator
                       , operand  :: Expression }
  deriving (Show)

-- TODO: add constructor fields for statements
data Statement
  = AssignStatement { assignedVar   :: Maybe Field
                    , assignedValue :: Expression }
  | BreakStatement
  | ReturnStatement
  | ControlFlowStatement
  deriving (Show)

data ControlFlowStatement
  = ForStatement
  | WhileStatement
  | IfStatement
  deriving (Show)

-- TODO
data Operator
  = Plus
  | Minus
  | Divide
  | Mod
  | Negate
  deriving (Show)

data Type =
  Type
  deriving (Show)

showName :: [String] -> String
showName l = intercalate "." l
