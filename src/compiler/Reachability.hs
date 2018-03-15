module Reachability (checkReturnAndReachability) where

import Data.Maybe
import JoosCompiler.Ast

checkReturnAndReachability :: AstNode -> [String]
checkReturnAndReachability tree = catMaybes $ map (\test -> test tree) tests
  where
    tests = []
