module Types
  ( getType
  ) where

import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Utils
import JoosCompiler.TreeUtils

getType :: WholeProgram -> Scope -> Expression -> Type
getType _ _ _ = Type Int False

isName :: InnerType -> Bool
isName (NamedType _) = True
isName _ = False

getLiteralType :: Literal -> Type
getLiteralType (IntegerLiteral _) = _type
  where
    _type = Type
               {innerType = Int
               ,isArray = False
               }
getLiteralType (BooleanLiteral _) = _type
  where
    _type = Type
               {innerType = Boolean
               ,isArray = False
               }

getLiteralType (CharacterLiteral _) = _type
  where
    _type = Type
               {innerType = Char
               ,isArray = False
               }

getLiteralType (StringLiteral _) = _type
  where
    _type = Type
               {innerType =  NamedType (["java","lang","String"])
               ,isArray = False
               }

getLiteralType (NullLiteral) = _type
  where
    _type = Null

getLiteralType _ = error "Unhandled case for getLiteralType"
