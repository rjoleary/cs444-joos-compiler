module Ast where

data Modifier = Public | Protected | Final | Abstract | Static | Native
type Name = [String]

data CompilationUnit = CompilationUnit
        { package   :: Maybe Name
        , imports   :: [Name]
        , classDecl :: ClassDeclaration
        }
        | EmptyFile

data ClassDeclaration = ClassDeclaration
        { isInterface    :: Bool
        , classModifiers :: [Modifier]
        , className      :: String
        , super          :: Name
        , interfaces     :: [Name]
        , fields         :: [Field]
        , methods        :: [Method]
        , constructor    :: Maybe Method
        }


data Field = Field
        { fieldType      :: Type
        , fieldModifiers :: [Modifier]
        , fieldName      :: Name
        , fieldValue     :: Expression
        }

-- TODO
data Method = Method
data Type = Type
data Expression = Expression
