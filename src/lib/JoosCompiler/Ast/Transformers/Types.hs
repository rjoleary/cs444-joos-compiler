module JoosCompiler.Ast.Transformers.Types where

import           Data.Tree
import           Data.Maybe
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions
import           JoosCompiler.Treeify
import           JoosCompiler.TreeUtils

data AstWrapper
  = AstTypeDeclaration { astClass :: TypeDeclaration }
  | AstBlock { astBlock :: Block }
  | AstWholeProgram { astWholeProgram :: WholeProgram }
  | AstCompilationUnit { astCompilationUnit :: CompilationUnit }
  | AstConstructor { astConstructor :: Method }
  | AstExpression { astExpression :: Expression }
  | AstField { astField :: Variable }
  | AstImport { astImport :: ImportDeclaration }
  | AstLocalVariable { astLocalVariable :: Variable }
  | AstMethod { astMethod :: Method }
  | AstModifier { astModifier :: Modifier }
  | AstModifiers { astModifiers :: [Modifier] }
  | AstPackage { astPackage :: Package }
  | AstPackageDeclaration { astPackageDeclaration :: PackageDeclaration }
  | AstStatement { astStatement :: Statement }
  | AstTaggedToken { astTaggedToken :: TaggedToken }
  | AstType { astType :: Type }
  deriving(Show)

type AstNode = Tree AstWrapper

children :: AstWrapper -> [AstWrapper]
children (AstTypeDeclaration x) = (map AstField $ classFields x) ++
                                  (map AstMethod $ methods x) ++
                                  (map AstMethod $ constructors x)
children (AstBlock x)           = []
children (AstWholeProgram (WholeProgram _ cus))    = (map AstCompilationUnit cus)
children (AstCompilationUnit x) = (map AstImport $ imports x) ++
                                  (map AstTypeDeclaration $ maybeToList $ typeDecl x)
children (AstConstructor x)     = error "AstConstructor not in final AST"
children (AstExpression (MethodInvocation x _ xs))  = AstExpression x : map AstExpression xs
children (AstExpression (BinaryOperation _ x y))    = map AstExpression [x, y]
children (AstExpression (UnaryOperation _ x))       = [AstExpression x]
children (AstExpression (LiteralExpression _))      = []
children (AstExpression This)                       = []
children (AstExpression (DynamicFieldAccess e _))   = [AstExpression e]
children (AstExpression (ExpressionName _))         = []
children (AstExpression (NewExpression name args))  = map AstExpression args
children (AstExpression (NewArrayExpression t e))   = [AstExpression e]
children (AstExpression (CastExpression t e))       = [AstExpression e, AstType t]
children (AstExpression (InstanceOfExpression e t)) = [AstExpression e, AstType t]
children (AstExpression (ArrayExpression e1 e2))    = [AstExpression e1, AstExpression e2]
children (AstField x)           = [ AstType $ variableType x
                                  , AstExpression $ variableValue x ]
children (AstImport x)          = []
-- TODO: this is part of statement
children (AstLocalVariable x)   = [ AstType $ variableType x
                                  , AstExpression $ variableValue x ]
children (AstMethod x)          = (map AstLocalVariable $ methodParameters x) ++
                                  [AstStatement $ methodStatement x]
children (AstModifier x)        = error "AstModifier not in final AST"
children (AstModifiers x)       = error "AstModifiers not in final AST"
children (AstPackageDeclaration x) = error "AstPackageDeclartion not in final AST"
children (AstStatement x@BlockStatement{}) =
  [ AstStatement $ statementBlock x
  , AstStatement $ nextStatement x ]
children (AstStatement x@ExpressionStatement{}) =
  [ AstExpression $ statementExpression x
  , AstStatement $ nextStatement x ]
children (AstStatement x@LoopStatement{}) =
  [ AstExpression $ loopPredicate x
  , AstStatement $ loopStatement x
  , AstStatement $ nextStatement x ]
children (AstStatement x@IfStatement{}) =
  [ AstExpression $ ifPredicate x
  , AstStatement $ ifThenStatement x
  , AstStatement $ ifElseStatement x
  , AstStatement $ nextStatement x ]
children (AstStatement x@ReturnStatement{}) =
  (fmap AstExpression $ maybeToList $ returnExpression x) ++
  [ AstStatement $ nextStatement x ]
children (AstStatement x@LocalStatement{}) =
  [ AstExpression $ variableValue $ localVariable x
  , AstStatement $ nextStatement x ]
children (AstStatement x@EmptyStatement{}) =
  [ AstStatement $ nextStatement x ]
children (AstStatement TerminalStatement) = []
children (AstTaggedToken x)     = error "AstTaggedToken not in final AST"
children (AstType x)            = []

-- Convert to Data.Tree representation.
asTree :: AstWrapper -> AstNode
asTree x = Node x (map asTree $ children x)

-- Convert to AstWrapper representation.
asAst :: AstNode -> AstWrapper
asAst = rootLabel

class AstWrappable a where
  wrap :: a -> AstWrapper

instance AstWrappable TypeDeclaration where
  wrap = AstTypeDeclaration

instance AstWrappable WholeProgram where
  wrap = AstWholeProgram

instance AstWrappable CompilationUnit where
  wrap = AstCompilationUnit

instance AstWrappable Expression where
  wrap = AstExpression

instance AstWrappable Variable where
  wrap = AstField

instance AstWrappable ImportDeclaration where
  wrap = AstImport

instance AstWrappable Method where
  wrap = AstMethod

instance AstWrappable Statement where
  wrap = AstStatement

instance AstWrappable Type where
  wrap = AstType


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

isExpression :: AstWrapper -> Bool
isExpression (AstExpression _) = True
isExpression _            = False

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

isStatement :: AstWrapper -> Bool
isStatement (AstStatement _) = True
isStatement _                = False


isType :: AstWrapper -> Bool
isType (AstType _) = True
isType _           = False
