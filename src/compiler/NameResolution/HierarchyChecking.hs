module NameResolution.HierarchyChecking
  ( checkHierarchy
  ) where

import Data.Tree
import Data.List
import Data.Function
import Data.List.Unique
import JoosCompiler.Ast
import JoosCompiler.Ast.NodeTypes
import JoosCompiler.Ast.Utils
import JoosCompiler.TreeUtils

-- TODO: fix name conflicts in records
implements = JoosCompiler.Ast.NodeTypes.interfaces

type TypeHierarchy = Tree TypeDeclaration

-- Returns a error message if hierarchy checking fails.
checkHierarchy :: AstNode -> Either String ()
checkHierarchy ast = do
    -- This check must be first; otherwise, the compiler might go into an infinite loop.
    ruleFor types "The hierarchy must be acyclic"
      (null . drop (length types) . levels . typeHierarchy)

    ruleFor classes "A class must not extend an interface"
      (not . isInterface . dumbResolve . super)

    ruleFor classes "A class must not implement a class"
      (and . map (isInterface . dumbResolve) . implements)

    ruleFor types "An interface must not be repeated in an implements or extends clause"
      (allUnique . map (typeName . dumbResolve) . implements)

    ruleFor classes "A class must not extend a final class"
      (not . isClassFinal . dumbResolve . super)

    ruleFor interfaces "An interface must not extend a class"
      (and . map (isInterface . dumbResolve) . implements)

    ruleFor types "A class or interface must not declare two methods with the same signature"
      (allUnique . map methodSignature . methods)

    ruleFor types "A class must not declare two constructors with the same parameter types"
      (allUnique . map methodSignature . constructors)

    ruleFor types "A class or interface must not inherit two methods with the same signature but different return types"
      (\t ->
        let returnAndSignature = map (\x -> (show $ methodType x, methodSignature x)) $ indirectMethods t
        in and $ map (\x -> all (==head x) x) $ map (map fst) $ groupBy (\x y -> snd x == snd y) $ sortBy (\x y -> snd x `compare` snd y) $ returnAndSignature
      )

    -- TODO: everything after this point has no tests
    ruleFor classes "A class that contains any abstract methods must be abstract"
      (\x -> (not $ hasAbstractMethod x) || isAbstractType x)

    ruleFor classes "A nonstatic method must not replace a static method"
      (\clazz ->
        let staticMethods = map methodSignature $ filter isMethodStatic $ indirectMethods clazz
        in and $ map (not . (`elem` staticMethods)) $ map methodSignature $ filter (not . isMethodStatic) $ methods clazz
      )

    ruleFor classes "A method must not replace a method with a different return type"
      (const True) -- TODO

    ruleFor classes "A protected method must not replace a public method"
      (\clazz ->
        let protectedMethods = map methodSignature $ filter isMethodPublic $ indirectMethods clazz
        in and $ map (not . (`elem` protectedMethods)) $ map methodSignature $ filter isMethodProtected $ methods clazz
      )

    ruleFor classes "A method must not replace a final method"
      (\clazz ->
        let finalMethods = map methodSignature $ filter isMethodFinal $ indirectMethods clazz
        in and $ map (not . (`elem` finalMethods)) $ map methodSignature $ methods clazz
      )

  where
    ruleFor types err f = asEither $ filter (not . f) $ types
      where asEither (x:_) = Left ("Error with type " ++ typeName x ++ ", \"" ++ err ++ "\"")
            asEither []    = Right ()

    indirectMethods :: TypeDeclaration -> [Method]
    indirectMethods x = concatMap methods (indirectExtends x ++ indirectImplements x)

    -- TODO: Object will be canonicalized to java.lang.Object
    indirectExtends :: TypeDeclaration -> [TypeDeclaration]
    indirectExtends TypeDeclaration{typeName="Object"} = []
    indirectExtends x = supers ++ concatMap (indirectExtends) supers
      where supers = [dumbResolve $ super x]

    indirectImplements :: TypeDeclaration -> [TypeDeclaration]
    indirectImplements x = implementers ++ concatMap indirectImplements implementers
      where implementers = map dumbResolve $ implements x

    typeHierarchy :: TypeDeclaration -> TypeHierarchy
    typeHierarchy x = typeHierarchy' x
      where
        typeHierarchy' TypeDeclaration{typeName="Object"} = createNode []
        typeHierarchy' TypeDeclaration{isInterface=True}  = createNode $ implements x
        typeHierarchy' TypeDeclaration{}                  = createNode $ super x:implements x
        createNode xs = Node x $ map (typeHierarchy . dumbResolve) xs

    -- TODO: make smart
    dumbResolve :: Name -> TypeDeclaration
    dumbResolve []   = dumbResolve ["Object"] -- TODO: make Object the default super
    dumbResolve name = head $ filter (\x -> typeName x == last name) $ types
      where head (x:_) = x
            head []    = error ("Could not resolve '" ++ showName name ++ "', type linking should have caught this")

    classes = filter (not . isInterface) $ types
    interfaces = filter isInterface $ types
    types = map (astClass . rootLabel) $ findChildren isTypeDeclaration ast

    isAbstractType :: TypeDeclaration -> Bool
    isAbstractType = (Abstract `elem`) . classModifiers

    hasAbstractMethod :: TypeDeclaration -> Bool
    hasAbstractMethod c =
      any (Abstract `elem`) $ map methodModifiers $ methods c
