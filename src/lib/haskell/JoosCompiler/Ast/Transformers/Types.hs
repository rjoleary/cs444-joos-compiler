module JoosCompiler.Ast.Transformers.Types where

import           Data.Tree
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

data AstWrapper
  = AstTypeDeclaration { astClass :: TypeDeclaration }
  | AstBlock { astBlock :: Block }
  | AstWholeProgram { astWholeProgram :: WholeProgram }
  | AstCompilationUnit { astCompilationUnit :: CompilationUnit }
  | AstConstructor { astConstructor :: Method }
  | AstConstructorBody { astConstructorBody :: Block }
  | AstField { astField :: Field }
  | AstImport { astImport :: ImportDeclaration }
  | AstLocalVariable { astLocalVariable :: Local }
  | AstMethod { astMethod :: Method }
  | AstMethodBody { astMethodBody :: Block }
  | AstModifier { astModifier :: Modifier }
  | AstModifiers { astModifiers :: [Modifier] }
  | AstPackage { astPackage :: Package }
  | AstPackageDeclaration { astPackageDeclaration :: PackageDeclaration }
  | AstStatement { astStatement :: Statement }
  | AstTaggedToken { astTaggedToken :: TaggedToken }
  | AstType { astType :: Type }
  deriving (Show)

type AstNode = Tree AstWrapper

type Transformer = [AstNode] -> TaggedParseTree -> AstWrapper

isBlock :: AstWrapper -> Bool
isBlock (AstBlock _) = True
isBlock _            = False

isTypeDeclaration :: AstWrapper -> Bool
isTypeDeclaration (AstTypeDeclaration _) = True
isTypeDeclaration _                      = False

isCompilationUnit :: AstWrapper -> Bool
isCompilationUnit (AstCompilationUnit _) = True
isCompilationUnit _                      = False

isConstructor :: AstWrapper -> Bool
isConstructor (AstConstructor _) = True
isConstructor _                  = False

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

isPackageDeclaration :: AstWrapper -> Bool
isPackageDeclaration (AstPackageDeclaration _) = True
isPackageDeclaration _                         = False

isType :: AstWrapper -> Bool
isType (AstType _) = True
isType _           = False

getType :: [AstNode] -> Type
getType ts = astType $ rootLabel typeNode
  where
    typeNode = head $ findChildren1 isType ts
