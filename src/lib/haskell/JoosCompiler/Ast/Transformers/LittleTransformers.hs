module JoosCompiler.Ast.Transformers.LittleTransformers where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.Transformers.Types
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

cstTransformer :: Transformer
cstTransformer _ (Node token _) = AstTaggedToken $ token
