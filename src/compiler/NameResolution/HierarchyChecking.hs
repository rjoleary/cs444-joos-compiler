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
    -- TODO: Hierarchy might not actually be acyclic.
    withErrorFor classes "The hierarchy must be acyclic"
      (\x -> (length $ take 1000 (indirectExtends x)) /= 1000)
    withErrorFor types "The hierarchy must be acyclic"
      (\x -> (length $ take 1000 (indirectImplements x)) /= 1000)

    withErrorFor classes "A class must not extend an interface"
      (not . isInterface . dumbResolve . super)

    withErrorFor classes "A class must not implement a class"
      (and . map (isInterface . dumbResolve) . implements)

    withErrorFor types "An interface must not be repeated in an implements or extends clause"
      (allUnique . map (className . dumbResolve) . implements)

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

    -- TODO: everything after this point has no tests
    withError "A class that contains any abstract methods must be abstract"
      True -- TODO

    withError "A nonstatic method must not replace a static method"
      True -- TODO

    withError "A method must not replace a method with a different return type"
      True -- TODO

    withErrorFor classes "A protected method must not replace a public method"
      (\clazz ->
        let finalMethods = map methodSignature $ filter isMethodPublic $ indirectMethods clazz
        in and $ map (not . (`elem` finalMethods)) $ map methodSignature $ filter isMethodProtected $ methods clazz
      )

    withErrorFor classes "A method must not replace a final method"
      (\clazz ->
        let finalMethods = map methodSignature $ filter isMethodFinal $ indirectMethods clazz
        in and $ map (not . (`elem` finalMethods)) $ map methodSignature $ methods clazz
      )

  where
    withError err x
      | x == False = Left err
      | otherwise  = Right ()

    withErrorFor xs err f = withError err $ and $ map f $ xs

    indirectMethods :: ClassDeclaration -> [Method]
    indirectMethods x = concatMap methods (indirectExtends x ++ indirectImplements x)

    -- TODO: Object will be canonicalized to java.lang.Object
    indirectExtends :: ClassDeclaration -> [ClassDeclaration]
    indirectExtends ClassDeclaration{className="Object"} = []
    indirectExtends x = supers ++ concatMap (indirectExtends) supers
      where supers = [dumbResolve $ super x]

    indirectImplements :: ClassDeclaration -> [ClassDeclaration]
    indirectImplements x = implementers ++ concatMap indirectImplements implementers
      where implementers = map dumbResolve $ implements x

    -- TODO: make smart
    dumbResolve :: Name -> ClassDeclaration
    dumbResolve []   = dumbResolve ["Object"] -- TODO: make Object the default super
    dumbResolve name = head $ filter (\x -> className x == last name) $ types
      where head (x:_) = x
            head []    = error ("Could not resolve '" ++ showName name ++ "', type linking should have caught this")

    classes = filter (not . isInterface) $ types
    interfaces = filter isInterface $ types
    types = map (astClass . rootLabel) $ findChildren isClassDeclaration ast
