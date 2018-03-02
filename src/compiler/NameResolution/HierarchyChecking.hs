module NameResolution.HierarchyChecking
  ( checkHierarchy
  ) where

import JoosCompiler.Ast

-- Returns a error message if hierarchy checking fails.
checkHierarchy :: AstNode -> Either String ()
checkHierarchy ast = do
    withError "A class must not extend an interface"
      True

    withError "A class must not implement a class"
      True

    withError "An interface must not be repeated in an implements clause"
      True

    withError "An interface must not be repeated in an extends clause"
      True

    withError "A class must not extend a final class"
      True

    withError "An interface must not extend a class"
      True

    withError "The hierarchy must be acyclic"
      True

    withError "A class or interface must not declare two methods with the same signature"
      True

    withError "A class must not declare two constructors with the same parameter types"
      True

    withError "A class or interface must not inherit two methods with the same signature"
      True

  where
    withError err f
      | f == False = Left err
      | otherwise  = Right ()
