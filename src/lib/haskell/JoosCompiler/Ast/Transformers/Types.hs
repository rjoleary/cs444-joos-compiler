module JoosCompiler.Ast.Transformers.Types where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

data AstWrapper
  = AstClassDeclaration { astClass :: ClassDeclaration }
  | AstBlock Block
  | AstWholeProgram WholeProgram
  | AstCompilationUnit CompilationUnit
  | AstField { astField :: Field }
  | AstImport { astImport :: ImportDeclaration }
  | AstLocalVariable { astLocalVariable :: Field }
  | AstMethod { astMethod :: Method }
  | AstModifier { astModifier :: Modifier }
  | AstModifiers { astModifiers :: [Modifier] }
  | AstPackage { astPackage :: PackageDeclaration }
  | AstStatement { astStatement :: Statement }
  | AstTaggedToken TaggedToken
  | AstType { astType :: Type }
  deriving (Show)

type AstNode = Tree AstWrapper

type Transformer = [AstNode] -> TaggedParseTree -> AstWrapper

isBlock :: AstWrapper -> Bool
isBlock (AstBlock _) = True
isBlock _            = False

isClassDeclaration :: AstWrapper -> Bool
isClassDeclaration (AstClassDeclaration _) = True
isClassDeclaration _                       = False

isField :: AstWrapper -> Bool
isField (AstField _) = True
isField _            = False

getFields :: [AstNode] -> [AstWrapper]
getFields ts = map rootLabel $ findChildren1 isField ts

isImport :: AstWrapper -> Bool
isImport (AstImport _) = True
isImport _             = False

isLocalVariable :: AstWrapper -> Bool
isLocalVariable (AstLocalVariable _) = True
isLocalVariable _                    = False

isMethod :: AstWrapper -> Bool
isMethod (AstMethod _) = True
isMethod _             = False

isModifier :: AstWrapper -> Bool
isModifier (AstModifier _) = True
isModifier _               = False

isModifiers :: AstWrapper -> Bool
isModifiers (AstModifiers _) = True
isModifiers _                = False

getModifiers :: [AstNode] -> AstWrapper
getModifiers ts = rootLabel $ head $ findChildren1 isModifiers ts

isPackage :: AstWrapper -> Bool
isPackage (AstPackage _) = True
isPackage _              = False

isType :: AstWrapper -> Bool
isType (AstType _) = True
isType _           = False

getType :: [AstNode] -> Type
getType ts = astType $ rootLabel typeNode
  where
    typeNode = head $ findChildren1 isType ts
