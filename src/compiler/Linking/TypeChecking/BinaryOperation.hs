module Linking.TypeChecking.BinaryOperation
  ( checkBinaryOperation
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
import Types

makeNonArrayType :: InnerType -> Type
makeNonArrayType t = Type  t False

numTypes = map makeNonArrayType [Int, Short, Byte]

checkBinaryOperation :: WholeProgram -> (Scope, Expression) -> Bool
checkBinaryOperation program (scope, (Expression _ (BinaryOperation operator e1 e2)))
  | operator `elem` [Multiply, Divide, Modulus, Add, Subtract] &&
    (getType program scope e1) `elem` numTypes &&
    (getType program scope e2) `elem` numTypes =
    True
