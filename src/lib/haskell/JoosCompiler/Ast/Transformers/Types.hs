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
  | AstConstructorBody { astConstructorBody :: Block }
  | AstExpression { astExpression :: Expression }
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
  deriving(Show)

type AstNode = Tree AstWrapper

children :: AstWrapper -> [AstWrapper]
children (AstTypeDeclaration x) = (map AstField $ classFields x) ++
                                  (map AstMethod $ methods x) ++
                                  (map AstMethod $ constructors x)
children (AstBlock x)           = []
children (AstWholeProgram x)    = (map AstPackage $ findPackages $ programPackages x)
  where findPackages (SubPackage x []) = maybeToList x
        findPackages (SubPackage x xs) = maybeToList x ++ concatMap (findPackages . snd) xs
children (AstCompilationUnit x) = (map AstImport $ imports x) ++
                                  (map AstTypeDeclaration $ maybeToList $ typeDecl x)
children (AstConstructor x)     = error "AstConstructor not in final AST"
children (AstConstructorBody x) = error "AstConstructorBody not in final AST"
children (AstExpression x)      = innerChildren $ innerExpression x
  where
    innerChildren (MethodInvocation _ xs)    = map AstExpression xs
    innerChildren (BinaryOperation _ x y)    = map AstExpression [x, y]
    innerChildren (UnaryOperation _ x)       = [AstExpression x]
    innerChildren (Literal t _)              = [AstType t]
    innerChildren This                       = []
    innerChildren (FieldAccess e _)          = [AstExpression e]
    innerChildren (ExpressionName _)         = []
    innerChildren (AssignmentExpression x y) = [AstExpression x, AstExpression y]
children (AstField x)           = [] -- TODO: expression
children (AstImport x)          = []
children (AstLocalVariable x)   = [] -- TODO: expression
children (AstMethod x)          = (map AstLocalVariable $ methodParameters x) ++
                                  (map AstStatement $ methodStatements x)
children (AstMethodBody x)      = error "AstMethodBody not in final AST"
children (AstModifier x)        = error "AstModifier not in final AST"
children (AstModifiers x)       = error "AstModifiers not in final AST"
children (AstPackage x)         = map (AstCompilationUnit . snd) $ packageCompilationUnits x
children (AstPackageDeclaration x) = error "AstPackageDeclartion not in final AST"
children (AstStatement x)       = innerChildren $ statement x
  where
    innerChildren x@BlockStatement{}      = map AstStatement $ blockStatements x
    innerChildren x@ExpressionStatement{} = [AstExpression $ statementExpression x]
    innerChildren x@LoopStatement{}       = (AstExpression $ loopPredicate x) :
                                            (map AstStatement $ loopStatements x)
    innerChildren x@IfStatement{}         = (AstExpression $ ifPredicate x) :
                                            (AstStatement $ ifThenStatement x) :
                                            [AstStatement $ ifElseStatement x]
    innerChildren x@EmptyStatement{}      = []
children (AstTaggedToken x)     = error "AstTaggedToken not in final AST"
children (AstType x)            = []

-- Convert to Data.Tree representation.
asTree :: AstWrapper -> AstNode
asTree x = Node x (map asTree $ children x)

-- Convert to AstWrapper representation.
asAst :: AstNode -> AstWrapper
asAst = rootLabel

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
