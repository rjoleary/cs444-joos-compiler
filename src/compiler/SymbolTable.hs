{-

    package X;
    import Y.B;
    public class A {
      public A() {}
      public A(A y) {}
      public int x;
      public int m(int x) { // 0
        int y = 0;
        int a = 1;
        int b = 2;
        int c = 3;
        for (int i=x; i>0; i=i-1) { // 0.0
          int z = 2;
          y=y+1;
        }
        for (int i=x; i>0; i=i-1) { // 0.1
          { // 0.1.0
            { // 0.1.0.0
              int z = 0;
            }
            { // 0.1.0.1
              int z = 0;
            }
          }
          y=y+1;
        }
        return y;
      }
    }

Generates the following symbol table:

    Package X                      X
    Import Y.B                     X.Y.B
    Class A                        X.A
    Field x                        X.A.x
    Constructor A()                X.A.A()
    Constructor A(int) X.A.A(int)  X.A.A(int)
    Parameter y X.A.A().y          X.A.A(A).0.y
    Method m(int) A                X.A.m(int)
    Parameter x                    X.A.m(int).0.x
    Local y                        X.A.m(int).0.y
    Local a                        X.A.m(int).0.a
    Local b                        X.A.m(int).0.b
    Local c                        X.A.m(int).0.c
    Local i                        X.A.m(int).0.0.i
    Local z                        X.A.m(int).0.0.z
    Local i                        X.A.m(int).0.1.i
    Local z                        X.A.m(int).0.1.0.0.z
    Local z                        X.A.m(int).0.1.0.1.z

The canonical name is always of one of these forms and must be unique:

    Package:     <package>
    Import:      <package>.<type>
    Class:       <package>.<type>
    Field:       <package>.<type>.<field>
    Constructor: <package>.<type>.<type>(<arguments>)
    Method:      <package>.<type>.<method>(<arguments>)
    Parameter:   <package>.<type>.<method>(<arguments>).0.<parameter>
    Local:       <package>.<type>.<method>(<arguments>).<scope>.<local>

The unnamed package has the name "_".

The top-most level of a function always has the scope # of 0. Each opening
brace '{' in the function appends a new scope with the number being one
greater than the previous scope at this level. For-loop declarations are
part of the inner scope.

A lookup of x in scope X.A.m(int).0.1.0 is performed with the following queries
on the table:

    1. X.A.m(int).0.1.0.x  => no match
    2. X.A.m(int).0.1.x    => no match
    3. X.A.m(int).0.x      => MATCH!

-}
module SymbolTable where

import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           JoosCompiler.Ast
import           JoosCompiler.Ast.NodeTypes
import           JoosCompiler.Ast.NodeFunctions

type SymbolTable = Map.Map Canonical Symbol

data SymbolType
  = PackageSymbol
  | Import
  | Class
  | Interface
  | Field
  | Constructor
  | Method
  | Parameter
  | Local
  deriving (Show)

data Canonical =
  Canonical Name
  deriving (Eq, Ord)

data Symbol = Symbol
  { symbolType      :: SymbolType
  , symbolName      :: String -- Concatenated with dots '.'
  , symbolCanonical :: Canonical
  , symbolNode      :: AstWrapper
  }

instance Show Symbol where
  show x = show (symbolType x) ++ " " ++ show (symbolCanonical x)

instance Show Canonical where
  show (Canonical name) = showName name

-- Left: String containing and error mesesage
-- Right: The created symbol table
type SymbolTableResult = Either String SymbolTable

createSymbolTable :: AstWrapper -> SymbolTableResult
createSymbolTable = createSymbolTable' (Canonical [])
  where
    createSymbolTable' :: Canonical -> AstWrapper -> SymbolTableResult
    -- createSymbolTable' canonical ast@(AstWholeProgram (WholeProgram xs)) =
    --   foldl merge empty $ map (createSymbolTable' canonical . AstPackage) xs

    createSymbolTable' canonical ast@(AstCompilationUnit x@CompilationUnit {}) =
      packageTable --`merge` createSymbolTable'
      where
        packageName = showName $ fromMaybe ["_"] (cuPackage x) -- '_' is the unamed package
        packageTable =
          singleton (createSymbol PackageSymbol packageName canonical ast)
        --importsTable = singleton (Symbol Import )
    --createSymbolTable' canonical (TypeDeclaration ast) = Right (Map.singleton)

    createSymbolTable' _ _ = empty
    createSymbol :: SymbolType -> String -> Canonical -> AstWrapper -> Symbol
    createSymbol symbolType name (Canonical prefix) ast =
      (Symbol symbolType name (Canonical (prefix ++ [name])) ast)

    empty :: SymbolTableResult
    empty = Right Map.empty

    singleton :: Symbol -> SymbolTableResult
    singleton symbol = Right $ Map.singleton (symbolCanonical symbol) symbol

    merge :: SymbolTableResult -> SymbolTableResult -> SymbolTableResult

    err@(Left _) `merge` _ = err
    _ `merge` err@(Left _) = err

    (Right x) `merge` (Right y)
      | Map.null (Map.intersection x y) = Right (Map.union x y)
      | otherwise = Left "Symbol conflict"
