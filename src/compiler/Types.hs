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

isNumber :: Type -> Bool
isNumber (Type Byte)  = True
isNumber (Type Int)   = True
isNumber (Type Short) = True
isNumber _            = False

getLiteralType :: Literal -> Type
getLiteralType = literalType
