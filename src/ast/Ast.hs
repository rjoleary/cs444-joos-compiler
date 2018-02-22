module Ast where

data Modifier = Public | Protected | Final | Abstract | Static | Native
type Name = [String]

data CompilationUnit = CompilationUnit
        { package   :: Maybe Name
        , imports   :: [Name]
        , classDecl :: ClassDeclaration
        }
        | EmptyFile

data Field = Field
        { fieldType      :: Type
        , fieldModifiers :: [Modifier]
        , fieldName      :: Name
        , fieldValue     :: Expression
        }

data Scope = Scope
        { parentScope :: Maybe Scope
        , scopeVariables :: [Field]
        , statements :: [Statement]
        }

data ClassDeclaration = ClassDeclaration
        { isInterface    :: Bool
        , classModifiers :: [Modifier]
        , className      :: String
        , super          :: Name
        , interfaces     :: [Name]
        , classScope     :: Scope
        , methods        :: [Method]
        , constructor    :: Maybe Method
        }

data Method = Method
        { methodType :: Type
        , methodModifiers :: [Modifier]
        , methodName :: Name
        -- list of scopes where scope n is a parent of scope n + 1
        -- (first methodScopes) is the scope of this method's class
        , methodScopes :: [Scope]
        }

data Expression = MethodInvocation
        { functionName :: Name
        , arguments :: [Expression]
        }
        | BinOpApplication
        { operator :: Operator
        , leftOperand :: Expression
        , rightOperand :: Expression
        }
        | UnaryOpApplication
        { operator :: Operator
        , operand :: Expression
        }


-- TODO: add constructor fields for statements
data Statement = AssignStatement
        { assignedVar :: Maybe Field
        , assignedValue :: Expression
        }
        | BreakStatement
        | ReturnStatement
        | ControlFlowStatement

data ControlFlowStatement = ForStatement
        | WhileStatement
        | IfStatement

-- TODO
data Type = Type
