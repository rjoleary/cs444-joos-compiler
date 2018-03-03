module NameResolution.HierarchyChecking
  ( checkHierarchy
  ) where

import Data.Tree
import Data.List.Unique
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Utils
import JoosCompiler.TreeUtils

-- TODO: fix name conflicts in records
implements = JoosCompiler.Ast.NodeTypes.interfaces

-- Returns a error message if hierarchy checking fails.
checkHierarchy :: AstNode -> Either String ()
checkHierarchy ast = do
    -- This check must be first; otherwise, the compiler might go into an infinite loop.
    withError "The hierarchy must be acyclic"
      True -- TODO

    withError "A class must not extend an interface"
      (and $ map (not . isInterface . dumbResolve . super) $ classes)

    withError "A class must not implement a class"
      (and $ map (and . map (isInterface . dumbResolve) . implements) $ classes)

    withError "An interface must not be repeated in an implements or extends clause"
      (and $ map (allUnique . implements) $ types)

    withError "A class must not extend a final class"
      True -- TODO

    withError "An interface must not extend a class"
      (and $ map (and . map (isInterface . dumbResolve) . implements) $ interfaces)

    withError "A class or interface must not declare two methods with the same signature"
      (and $ map (allUnique . map methodSignature . methods) $ types)

    withError "A class must not declare two constructors with the same parameter types"
      (and $ map (allUnique . map methodSignature . constructors) $ types)

    withError "A class or interface must not inherit two methods with the same signature but different return types"
      True -- TODO

  where
    withError err f
      | f == False = Left err
      | otherwise  = Right ()

    indirectMethods :: ClassDeclaration -> [Method]
    indirectMethods x = []

    indirectExtends :: ClassDeclaration -> [ClassDeclaration]
    indirectExtends x = []

    indirectImplements :: ClassDeclaration -> [ClassDeclaration]
    indirectImplements x = []

    -- TODO: make smart
    dumbResolve :: Name -> ClassDeclaration
    dumbResolve []   = dumbResolve ["Object"] -- TODO: make Object the default super
    dumbResolve name = head $ filter (\x -> className x == last name) $ classes
      where head (x:_) = x
            head []    = error ("Could not resolve '" ++ showName name ++ "', type linking should have caught this")

    classes = filter (not . isInterface) $ types
    interfaces = filter isInterface $ types
    types = map (astClass . rootLabel) $ findChildren isClassDeclaration ast
