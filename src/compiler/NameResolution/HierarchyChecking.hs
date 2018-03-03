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

    withErrorFor classes "A class must not extend an interface"
      (not . isInterface . dumbResolve . super)

    withErrorFor classes "A class must not implement a class"
      (and . map (isInterface . dumbResolve) . implements)

    withErrorFor types "An interface must not be repeated in an implements or extends clause"
      (allUnique . map dumbResolve . implements)

    withErrorFor classes "A class must not extend a final class"
      (not . isClassFinal . dumbResolve . super)

    withErrorFor interfaces "An interface must not extend a class"
      (and . map (isInterface . dumbResolve) . implements)

    withErrorFor types "A class or interface must not declare two methods with the same signature"
      (allUnique . map methodSignature . methods)

    withErrorFor types "A class must not declare two constructors with the same parameter types"
      (allUnique . map methodSignature . constructors)

    withError "A class or interface must not inherit two methods with the same signature but different return types"
      True -- TODO

  where
    withError err x
      | x == False = Left err
      | otherwise  = Right ()

    withErrorFor xs err f = withError err $ and $ map f $ xs

    indirectMethods :: ClassDeclaration -> [Method]
    indirectMethods x = []

    indirectExtends :: ClassDeclaration -> [ClassDeclaration]
    indirectExtends x = []

    indirectImplements :: ClassDeclaration -> [ClassDeclaration]
    indirectImplements x = []

    -- TODO: make smart
    dumbResolve :: Name -> ClassDeclaration
    dumbResolve []   = dumbResolve ["Object"] -- TODO: make Object the default super
    dumbResolve name = head $ filter (\x -> className x == last name) $ types
      where head (x:_) = x
            head []    = error ("Could not resolve '" ++ showName name ++ "', type linking should have caught this")

    classes = filter (not . isInterface) $ types
    interfaces = filter isInterface $ types
    types = map (astClass . rootLabel) $ findChildren isClassDeclaration ast
