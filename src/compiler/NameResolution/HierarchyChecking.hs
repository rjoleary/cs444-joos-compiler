module NameResolution.HierarchyChecking
  ( checkHierarchy
  ) where

import Data.Function
import Data.List
import Data.List.Unique
import Data.Maybe
import Data.Tree
import Debug.DumbTrace(trace)
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.NodeFunctions
import JoosCompiler.Ast.Utils
import JoosCompiler.TreeUtils

-- TODO: fix name conflicts in records
implements = JoosCompiler.Ast.NodeTypes.interfaces

type TypeHierarchy = Tree TypeDeclaration

-- Returns a error message if hierarchy checking fails.
checkHierarchy :: AstNode -> Either String ()
checkHierarchy ast@(Node (AstWholeProgram program) _) = do
    -- This check must be first; otherwise, the compiler might go into an infinite loop.
    ruleFor types "The hierarchy must be acyclic"
      (null . drop (length types) . levels . typeHierarchy)

    ruleFor classes "A class must not extend an interface"
      (not . isInterface . smartResolve . super)

    ruleFor classes "A class must not implement a class"
      (and . map (isInterface . smartResolve) . implements)


    ruleFor types "An interface must not be repeated in an implements or extends clause"
      (\t -> let names = implements t in
          (allUnique $ trace (show $ map showName names) names))

    ruleFor classes "A class must not extend a final class"
      (not . isClassFinal . smartResolve . super)

    ruleFor interfaces "An interface must not extend a class"
      (and . map (isInterface . smartResolve) . implements)

    ruleFor types "A class or interface must not declare two methods with the same signature"
      (allUnique . map methodSignature . methods)

    ruleFor types "A class must not declare two constructors with the same parameter types"
      (allUnique . map methodSignature . constructors)

    -- This performs two checks from A2:
    -- * "A method must not replace a method with a different return type"
    -- * "A class or interface must not inherit two methods with the same signature but different return types"
    ruleFor types "A type must not replace or inherit two method with the same signature but different return types"
      (\t ->
        let ms  = methods t ++ indirectMethods t -- TODO: resolve types
            ret = methodReturn
            sig = methodSignature
        in and [ (sig x == sig y) `implies` (ret x == ret y) | (x:rest) <- tails ms, y <- rest ]
      )

    ruleFor concreteClasses "A class that contains any abstract methods must be abstract"
      (and . map (not . isMethodAbstract) . foldMethods)

    ruleFor classes "A nonstatic method must not replace a static method"
      (\clazz ->
        let staticMethods = map methodSignature $ filter isMethodStatic $ indirectMethods clazz
        in and $ map (not . (`elem` staticMethods)) $ map methodSignature $ filter (not . isMethodStatic) $ methods clazz
      )

    ruleFor classes "A protected method must not replace a public method"
      (\clazz ->
        let publicMethods = map methodSignature $ filter isMethodPublic $ indirectMethods clazz
        in and $ map (not . (`elem` publicMethods)) $ map methodSignature $ filter isMethodProtected $ methods clazz
      )

    ruleFor classes "A method must not replace a final method"
      (\clazz ->
        let finalMethods = map methodSignature $ filter isMethodFinal $ indirectMethods clazz
        in and $ map (not . (`elem` finalMethods)) $ map methodSignature $ methods clazz
      )

  where
    ruleFor types err f = asEither $ filter (not . f) $ types
      where asEither (x:_) = Left ("Error with type " ++ showName (typeCanonicalName x) ++ ", \"" ++ err ++ "\"")
            asEither []    = Right ()

    False `implies` False = True
    False `implies` True  = True
    True `implies` False  = False
    True `implies` True   = True

    -- Returns all the methods a type contains (declares + inherits - duplicates).
    foldMethods :: TypeDeclaration -> [Method]
    foldMethods x = nubBy ((==) `on` methodSignature) $ (methods x++) $ indirectMethods x

    indirectMethods :: TypeDeclaration -> [Method]
    indirectMethods x = concatMap methods $ tail $ flatten $ typeHierarchy x

    typeHierarchy :: TypeDeclaration -> TypeHierarchy
    typeHierarchy x = typeHierarchy' x
      where
        typeHierarchy' TypeDeclaration{typeCanonicalName=["java", "lang", "Object"]} = createNode []
        typeHierarchy' TypeDeclaration{isInterface=True}                             = createNode $ implements x
        typeHierarchy' TypeDeclaration{}                                             = createNode $ super x:implements x
        createNode xs = Node x $ map (typeHierarchy . smartResolve) xs

    smartResolve :: Name -> TypeDeclaration
    smartResolve []   = smartResolve ["java", "lang", "Object"]
    smartResolve name = fromMaybe
                       (error $ "Could not resolve type: " ++ showName name) $
                       resolveTypeInProgram program name

    classes = filter (not . isInterface) $ types
    concreteClasses = filter (not . (Abstract `elem`) . classModifiers) classes
    interfaces = filter isInterface $ types
    types = getTypeDeclarationsFromProgram program
